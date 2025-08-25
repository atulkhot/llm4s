package org.llm4s.trace

import org.llm4s.agent.AgentState
import org.llm4s.config.EnvLoader
import org.llm4s.llmconnect.model.{ AssistantMessage, SystemMessage, ToolMessage, UserMessage }
import org.slf4j.LoggerFactory

import java.time.Instant
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.util.{ Failure, Success, Try }

class LangfuseTracing(
  langfuseUrl: String = EnvLoader.getOrElse("LANGFUSE_URL", "https://cloud.langfuse.com/api/public/ingestion"),
  publicKey: String = EnvLoader.getOrElse("LANGFUSE_PUBLIC_KEY", ""),
  secretKey: String = EnvLoader.getOrElse("LANGFUSE_SECRET_KEY", ""),
  environment: String = EnvLoader.getOrElse("LANGFUSE_ENV", "production"),
  release: String = EnvLoader.getOrElse("LANGFUSE_RELEASE", "1.0.0"),
  version: String = EnvLoader.getOrElse("LANGFUSE_VERSION", "1.0.0")
) extends Tracing {
  private val logger         = LoggerFactory.getLogger(getClass)
  private def nowIso: String = DateTimeFormatter.ISO_INSTANT.format(Instant.now())
  private def uuid: String   = UUID.randomUUID().toString

  private def sendBatch(events: Seq[ujson.Obj]): Unit = {
    if (publicKey.isEmpty || secretKey.isEmpty) {
      logger.warn("[Langfuse] Public or secret key not set in environment. Skipping export.")
      logger.warn(s"[Langfuse] Expected environment variables: LANGFUSE_PUBLIC_KEY, LANGFUSE_SECRET_KEY")
      logger.warn(s"[Langfuse] Current URL: $langfuseUrl")
      return
    }

    logger.debug(s"[Langfuse] Sending batch to URL: $langfuseUrl")
    logger.debug(s"[Langfuse] Using public key: ${publicKey.take(10)}...")
    logger.debug(s"[Langfuse] Events in batch: ${events.length}")

    val batchPayload = ujson.Obj("batch" -> ujson.Arr(events: _*))

    Try {
      val response = requests.post(
        langfuseUrl,
        data = batchPayload.render(),
        headers = Map(
          "Content-Type" -> "application/json",
          "User-Agent"   -> "llm4s-scala/1.0.0"
        ),
        auth = (publicKey, secretKey),
        readTimeout = 30000,
        connectTimeout = 30000
      )

      if (response.statusCode == 207 || (response.statusCode >= 200 && response.statusCode < 300)) {
        logger.info(s"[Langfuse] Batch export successful: ${response.statusCode}")
        if (response.statusCode == 207) {
          logger.info(s"[Langfuse] Partial success response: ${response.text()}")
        }
      } else {
        logger.error(s"[Langfuse] Batch export failed: ${response.statusCode}")
        logger.error(s"[Langfuse] Response body: ${response.text()}")
        logger.error(s"[Langfuse] Request URL: $langfuseUrl")
        logger.error(s"[Langfuse] Request payload size: ${batchPayload.render().length} bytes")
      }
    } match {
      case Failure(e) =>
        logger.error(s"[Langfuse] Batch export failed with exception: ${e.getMessage}", e)
        logger.error(s"[Langfuse] Request URL: $langfuseUrl")
      case Success(_) =>
    }
  }

  override def traceAgentState(state: AgentState): Unit = {
    logger.info("[LangfuseTracing] Exporting agent state to Langfuse.")
    val traceId     = uuid
    val now         = nowIso

    // Trace-create event
    val modelName = state.conversation.messages
      .collectFirst {
        case am: AssistantMessage if am.toolCalls.nonEmpty => am
      }
      .flatMap(_.toolCalls.headOption.map(_.name))
      .getOrElse("unknown-model")
    val traceInput  = if (state.userQuery.nonEmpty) state.userQuery else "No user query"
    val traceOutput = state.conversation.messages.lastOption.map(_.content).filter(_.nonEmpty).getOrElse("No output")
    val traceEvent = TraceEvent.createTraceEvent(
      traceId = traceId,
      now = now,
      environment = environment,
      release = release,
      version = version,
      traceInput = traceInput,
      traceOutput = traceOutput,
      modelName = modelName,
      messageCount = state.conversation.messages.length
    )
    val batchEvents = List(traceEvent)

    // Observation events for each message
    val allBatchEvents = state.conversation.messages.zipWithIndex.foldLeft(batchEvents :+ traceEvent) { case (_, (msg, idx)) =>
      msg match {
        case am: AssistantMessage if am.toolCalls.nonEmpty =>
          // Get conversation context leading up to this generation
          val contextMessages = state.conversation.messages.take(idx)
          val generationEvent = am.toGenerationEventWithTools(uuid = uuid, traceId = traceId, idx = idx, now = now, modelName = modelName, contextMessages = contextMessages)
          batchEvents :+ generationEvent
        case am: AssistantMessage =>
          // Handle regular assistant messages without tool calls
          val contextMessages = state.conversation.messages.take(idx)
          val generationEvent = am.toGenerationEvent(uuid = uuid, traceId = traceId, idx = idx, now = now, modelName = modelName, contextMessages = contextMessages)
          batchEvents :+ generationEvent
        case tm: ToolMessage =>
          // Find the corresponding tool call for this tool message
          val contextMessages = state.conversation.messages.take(idx)
          val toolCallName = tm.findToolCallName(contextMessages)
          val spanEvent = tm.toSpanEvent(uuid = uuid, traceId = traceId, idx = idx, now = now, toolCallName = toolCallName)
          batchEvents :+ spanEvent
        case userMsg: UserMessage =>
          val eventEvent = userMsg.toEventCreate(uuid = uuid, traceId = traceId, idx = idx, now = now)
          batchEvents :+ eventEvent
        case sysMsg: SystemMessage =>
          val eventEvent = sysMsg.toEventCreate(uuid = uuid, traceId = traceId, idx = idx, now = now)
          batchEvents :+ eventEvent
        case _ =>
          val eventEvent = msg.toEventCreate(uuid = uuid, traceId = traceId, idx = idx, now = now)
          batchEvents :+ eventEvent
      }
    }
    sendBatch(allBatchEvents)
  }

  override def traceEvent(event: String): Unit = {
    logger.info(s"[LangfuseTracing] Event: $event")
    val eventObj = ujson.Obj(
      "id"        -> uuid,
      "timestamp" -> nowIso,
      "type"      -> "event-create",
      "body" -> ujson.Obj(
        "id"        -> uuid,
        "timestamp" -> nowIso,
        "name"      -> "Custom Event",
        "input"     -> ujson.Obj("event" -> event),
        "metadata"  -> ujson.Obj("source" -> "traceEvent")
      )
    )
    sendBatch(Seq(eventObj))
  }

  override def traceToolCall(toolName: String, input: String, output: String): Unit = {
    logger.info(s"[LangfuseTracing] Tool call: $toolName, input: $input, output: $output")
    val eventObj = ujson.Obj(
      "id"        -> uuid,
      "timestamp" -> nowIso,
      "type"      -> "span-create",
      "body" -> ujson.Obj(
        "id"        -> uuid,
        "timestamp" -> nowIso,
        "name"      -> s"Tool Call: $toolName",
        "input"     -> ujson.Obj("arguments" -> input),
        "output"    -> ujson.Obj("result" -> output),
        "metadata"  -> ujson.Obj("toolName" -> toolName)
      )
    )
    sendBatch(Seq(eventObj))
  }

  override def traceError(error: Throwable): Unit = {
    logger.error("[LangfuseTracing] Error occurred", error)
    val eventObj = ujson.Obj(
      "id"        -> uuid,
      "timestamp" -> nowIso,
      "type"      -> "event-create",
      "body" -> ujson.Obj(
        "id"            -> uuid,
        "timestamp"     -> nowIso,
        "name"          -> "Error",
        "level"         -> "ERROR",
        "statusMessage" -> error.getMessage,
        "input"         -> ujson.Obj("error" -> error.getMessage),
        "metadata" -> ujson.Obj(
          "errorType"  -> error.getClass.getSimpleName,
          "stackTrace" -> error.getStackTrace.take(5).mkString("\n")
        )
      )
    )
    sendBatch(Seq(eventObj))
  }

  override def traceCompletion(completion: org.llm4s.llmconnect.model.Completion, model: String): Unit = {
    logger.info(s"[LangfuseTracing] Completion: model=$model, id=${completion.id}")

    val now = nowIso

    // Create meaningful input structure
    val completionInput = ujson.Obj(
      "model"         -> model,
      "completion_id" -> completion.id,
      "created"       -> completion.created
    )

    // Create proper output structure with complete message content
    val completionOutput = ujson.Obj(
      "role"    -> completion.message.role,
      "content" -> completion.message.content
    )

    // Add tool calls if present
    if (completion.message.toolCalls.nonEmpty) {
      completionOutput("tool_calls") = ujson.Arr(
        completion.message.toolCalls.map(tc =>
          ujson.Obj(
            "id"   -> tc.id,
            "type" -> "function",
            "function" -> ujson.Obj(
              "name"      -> tc.name,
              "arguments" -> tc.arguments.render()
            )
          )
        ): _*
      )
    }

    val generationEvent = ujson.Obj(
      "id"        -> uuid,
      "timestamp" -> now,
      "type"      -> "generation-create",
      "body" -> ujson.Obj(
        "id"        -> uuid,
        "timestamp" -> now,
        "name"      -> "LLM Completion",
        "startTime" -> now,
        "endTime"   -> now,
        "model"     -> model,
        "input"     -> completionInput,
        "output"    -> completionOutput,
        "usage" -> completion.usage
          .map { usage =>
            ujson.Obj(
              "promptTokens"     -> usage.promptTokens,
              "completionTokens" -> usage.completionTokens,
              "totalTokens"      -> usage.totalTokens
            )
          }
          .getOrElse(ujson.Null),
        "metadata" -> ujson.Obj(
          "completionId"  -> completion.id,
          "created"       -> completion.created,
          "toolCallCount" -> completion.message.toolCalls.length,
          "standalone"    -> true
        )
      )
    )
    sendBatch(Seq(generationEvent))
  }

  override def traceTokenUsage(usage: org.llm4s.llmconnect.model.TokenUsage, model: String, operation: String): Unit = {
    logger.info(s"[LangfuseTracing] Token usage: $operation with $model - ${usage.totalTokens} tokens")

    val eventObj = ujson.Obj(
      "id"        -> uuid,
      "timestamp" -> nowIso,
      "type"      -> "event-create",
      "body" -> ujson.Obj(
        "id"        -> uuid,
        "timestamp" -> nowIso,
        "name"      -> s"Token Usage - $operation",
        "input" -> ujson.Obj(
          "model"     -> model,
          "operation" -> operation
        ),
        "output" -> ujson.Obj(
          "promptTokens"     -> usage.promptTokens,
          "completionTokens" -> usage.completionTokens,
          "totalTokens"      -> usage.totalTokens
        ),
        "metadata" -> ujson.Obj(
          "model"     -> model,
          "operation" -> operation,
          "tokenType" -> "usage"
        )
      )
    )
    sendBatch(Seq(eventObj))
  }
}
