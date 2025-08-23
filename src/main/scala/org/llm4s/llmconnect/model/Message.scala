package org.llm4s.llmconnect.model

/**
 * Represents a message in a conversation with an LLM (Large Language Model).
 */
sealed trait Message {
  def role: String
  def content: String

  override def toString: String = s"${role}: ${content}"

  def toEventCreate(
                     uuid: String,
                     traceId: String,
                     idx: Int,
                     now: String
                   ): ujson.Obj = {
    ujson.Obj(
      "id" -> uuid,
      "timestamp" -> now,
      "type" -> "event-create",
      "body" -> ujson.Obj(
        "id" -> s"${traceId}-event-$idx",
        "traceId" -> traceId,
        "name" -> s"Message $idx: $role",
        "startTime" -> now,
        "input" -> ujson.Obj("content" -> content),
        "metadata" -> ujson.Obj(
          "role" -> role
        )
      )
    )
  }

}

/**
 * Represents a user message in the conversation.
 *
 * @param content Content of the user message.
 */
case class UserMessage(content: String) extends Message {
  val role = "user"

  def toEventCreate(
                     uuid: String,
                     traceId: String,
                     idx: Int,
                     now: String
                   ): ujson.Obj = {
    ujson.Obj(
      "id" -> uuid,
      "timestamp" -> now,
      "type" -> "event-create",
      "body" -> ujson.Obj(
        "id" -> s"${traceId}-event-$idx",
        "traceId" -> traceId,
        "name" -> s"User Input $idx",
        "startTime" -> now,
        "input" -> ujson.Obj("content" -> content),
        "metadata" -> ujson.Obj(
          "role" -> role
        )
      )
    )
  }

}

/**
 * Represents a system message, which is typically used to set context or instructions for the LLM.
 *
 * A system prompt provides the foundational instructions and behavioral guidelines that shape how the
 * LLM should respond to a user request, including its personality, capabilities, constraints, and communication style.
 * It acts as the model's "operating manual," establishing context about what it should and shouldn't do,
 * how to handle various scenarios, and what information it has access to.
 *
 * @param content Content of the system message.
 */
case class SystemMessage(content: String) extends Message {
  val role = "system"

  def toEventCreate(
                     uuid: String,
                     traceId: String,
                     idx: Int,
                     now: String
                   ): ujson.Obj = {
    ujson.Obj(
      "id" -> uuid,
      "timestamp" -> now,
      "type" -> "event-create",
      "body" -> ujson.Obj(
        "id" -> s"${traceId}-event-$idx",
        "traceId" -> traceId,
        "name" -> s"System Message $idx",
        "startTime" -> now,
        "input" -> ujson.Obj("content" -> content),
        "metadata" -> ujson.Obj(
          "role" -> role
        )
      )
    )
  }

}

object AssistantMessage {
  def apply(content: String): AssistantMessage =
    AssistantMessage(Some(content), Seq.empty)
  def apply(content: String, toolCalls: Seq[ToolCall]): AssistantMessage =
    AssistantMessage(Some(content), toolCalls)
}

/**
 * Represents a message from the LLM assistant, which may include text, tool calls or both.
 *
 * @param contentOpt Optional content of the message.
 * @param toolCalls  Sequence of tool calls made by the assistant.
 */
case class AssistantMessage(
  contentOpt: Option[String] = None,
  toolCalls: Seq[ToolCall] = Seq.empty
) extends Message {
  val role = "assistant"

  def content: String = contentOpt.getOrElse("")

  override def toString: String = {
    val toolCallsStr = if (toolCalls.nonEmpty) {
      s"\nTool Calls: ${toolCalls.map(tc => s"[${tc.id}: ${tc.name}(${tc.arguments})]").mkString(", ")}"
    } else " - no tool calls"

    s"${role}: ${content}${toolCallsStr}"
  }

  def toGenerationEvent(
                         uuid: String,
                         traceId: String,
                         idx: Int,
                         now: String,
                         modelName: String,
                         contextMessages: Seq[Message]
                       ): ujson.Obj = {
    val conversationInput = contextMessages.map(msg =>
      ujson.Obj(
        "role" -> msg.role,
        "content" -> msg.content
      )
    )

    val generationOutput = ujson.Obj(
      "role" -> "assistant",
      "content" -> content
    )

    ujson.Obj(
      "id" -> uuid,
      "timestamp" -> now,
      "type" -> "generation-create",
      "body" -> ujson.Obj(
        "id" -> s"${traceId}-gen-$idx",
        "traceId" -> traceId,
        "name" -> s"LLM Generation $idx",
        "startTime" -> now,
        "endTime" -> now,
        "input" -> ujson.Arr(conversationInput: _*),
        "output" -> generationOutput,
        "model" -> modelName,
        "modelParameters" -> ujson.Obj(),
        "metadata" -> ujson.Obj(
          "messageIndex" -> idx,
          "toolCallCount" -> 0
        )
      )
    )
  }

  def toGenerationEventWithTools(
                                  uuid: String,
                                  traceId: String,
                                  idx: Int,
                                  now: String,
                                  modelName: String,
                                  contextMessages: Seq[Message]
                                ): ujson.Obj = {
    val conversationInput = contextMessages.map(msg =>
      ujson.Obj(
        "role" -> msg.role,
        "content" -> msg.content
      )
    )

    val generationOutput = ujson.Obj(
      "role" -> "assistant",
      "content" -> content,
      "tool_calls" -> ujson.Arr(
        toolCalls.map(tc =>
          ujson.Obj(
            "id" -> tc.id,
            "type" -> "function",
            "function" -> ujson.Obj(
              "name" -> tc.name,
              "arguments" -> tc.arguments.render()
            )
          )
        ): _*
      )
    )

    ujson.Obj(
      "id" -> uuid,
      "timestamp" -> now,
      "type" -> "generation-create",
      "body" -> ujson.Obj(
        "id" -> s"${traceId}-gen-$idx",
        "traceId" -> traceId,
        "name" -> s"LLM Generation $idx",
        "startTime" -> now,
        "endTime" -> now,
        "input" -> ujson.Arr(conversationInput: _*),
        "output" -> generationOutput,
        "model" -> modelName,
        "modelParameters" -> ujson.Obj(),
        "metadata" -> ujson.Obj(
          "messageIndex" -> idx,
          "toolCallCount" -> toolCalls.length
        )
      )
    )
  }

}

/**
 * Represents a message from a tool, typically containing the result of a tool call.
 *
 * @param toolCallId Unique identifier for the tool call (as provided by the ToolCall).
 * @param content    Content of the tool message, usually the result of the tool execution, e.g. a json response.
 */
case class ToolMessage(
  toolCallId: String,
  content: String
) extends Message {
  val role = "tool"

  override def toString: String = s"${role}(${toolCallId}): ${content}"

  def toSpanEvent(
                   uuid: String,
                   traceId: String,
                   idx: Int,
                   now: String,
                   toolCallName: String
                 ): ujson.Obj = {
    ujson.Obj(
      "id" -> uuid,
      "timestamp" -> now,
      "type" -> "span-create",
      "body" -> ujson.Obj(
        "id" -> s"${traceId}-span-$idx",
        "traceId" -> traceId,
        "name" -> s"Tool: $toolCallName",
        "startTime" -> now,
        "endTime" -> now,
        "input" -> ujson.Obj(
          "toolCallId" -> toolCallId,
          "toolName" -> toolCallName
        ),
        "output" -> ujson.Obj(
          "result" -> content
        ),
        "metadata" -> ujson.Obj(
          "role" -> role,
          "toolCallId" -> toolCallId,
          "toolName" -> toolCallName
        )
      )
    )
  }

}

/**
 * Represents a tool call request from the LLM.
 *
 * @param id Unique identifier for the tool call (generated byt the LLM).
 * @param name Name of the tool being called. (from the list of tools provided to the LLM).
 * @param arguments Arguments passed to the tool in JSON format.
 */
case class ToolCall(
  id: String,
  name: String,
  arguments: ujson.Value
) {
  override def toString: String = s"ToolCall($id, $name, $arguments)"
}
