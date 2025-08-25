
package org.llm4s.trace

import org.llm4s.agent.AgentState
import org.llm4s.llmconnect.model._
import org.llm4s.toolapi.ToolRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LangfuseTracingSpec extends AnyFlatSpec with Matchers {

  private val emptyToolRegistry = new ToolRegistry(Seq.empty)

  "LangfuseTracing.traceAgentState" should "create correct trace for empty conversation" in {
    val mockTracing = new MockLangfuseTracing()
    val emptyState = AgentState(
      conversation = Conversation(Seq.empty),
      userQuery = "",
      tools = emptyToolRegistry
    )

    mockTracing.traceAgentState(emptyState)

    val events = mockTracing.getSentBatchEvents
    events.length shouldBe 1 // Should have one trace-create event

    val traceEvent = events.head
    (traceEvent.obj("body")("modelName").str) shouldBe "unknown-model"
    (traceEvent.obj("body")("input").str) shouldBe "No user query"
    (traceEvent.obj("body")("output").str) shouldBe "No output"
    (traceEvent.obj("body")("messageCount").num) shouldBe 0
  }

  it should "create correct trace for conversation with system and user messages" in {
    val mockTracing = new MockLangfuseTracing()
    val state = AgentState(
      conversation = Conversation(Seq(
        SystemMessage("System prompt"),
        UserMessage("User question")
      )),
      userQuery = "User question",
      tools = emptyToolRegistry
    )

    mockTracing.traceAgentState(state)

    val events = mockTracing.getSentBatchEvents
    events.length shouldBe 3 // trace-create + 2 message events

    val traceEvent = events.head
    (traceEvent.obj("body")("input").str) shouldBe "User question"
    (traceEvent.obj("body")("messageCount").num) shouldBe 2
  }

  it should "create correct trace with tool calls" in {
    val mockTracing = new MockLangfuseTracing()
    val toolCall = ToolCall("id1", "gpt-4", ujson.Obj("param" -> "value"))
    val state = AgentState(
      conversation = Conversation(Seq(
        SystemMessage("System prompt"),
        UserMessage("User question"),
        AssistantMessage(Some("Using tool"), Seq(toolCall)),
        ToolMessage("id1", """{"result": "ok"}""")
      )),
      userQuery = "User question",
      tools = emptyToolRegistry
    )

    mockTracing.traceAgentState(state)

    val events = mockTracing.getSentBatchEvents
    events.length shouldBe 5 // trace-create + 4 message events

    val traceEvent = events.head
    (traceEvent.obj("body")("modelName").str) shouldBe "gpt-4"
    (traceEvent.obj("body")("messageCount").num) shouldBe 4
  }

  it should "handle messages with empty content" in {
    val mockTracing = new MockLangfuseTracing()
    val state = AgentState(
      conversation = Conversation(Seq(
        SystemMessage(""),
        AssistantMessage(None, Seq.empty)
      )),
      userQuery = "Test query",
      tools = emptyToolRegistry
    )

    mockTracing.traceAgentState(state)

    val events = mockTracing.getSentBatchEvents
    val traceEvent = events.head
    (traceEvent.obj("body")("output").str) shouldBe "No output"
  }
}

// Mock implementation for testing
private class MockLangfuseTracing extends LangfuseTracing(
  langfuseUrl = "mock-url",
  publicKey = "mock-key",
  secretKey = "mock-secret"
) {
  private var sentBatchEvents = Seq.empty[ujson.Obj]

  protected def sendBatch(events: Seq[ujson.Obj]): Unit = {
    sentBatchEvents = events
  }

  def getSentBatchEvents: Seq[ujson.Obj] = sentBatchEvents
}