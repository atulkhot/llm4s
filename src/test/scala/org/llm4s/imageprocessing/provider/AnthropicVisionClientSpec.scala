package org.llm4s.imageprocessing.provider

import org.llm4s.imageprocessing.config.AnthropicVisionConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AnthropicVisionClientSpec extends AnyFlatSpec with Matchers {

  // Create an instance with minimal config for testing
  val config = AnthropicVisionConfig("dummy-key", "https://dummy-url", "claude-3")
  val client = new AnthropicVisionClient(config)

  "extractContentFromResponse" should "extract simple text content from JSON response" in {
    val jsonResponse = """{"text": "Hello world"}"""
    val result       = client.extractContentFromResponse(jsonResponse)
    result shouldBe "Hello world"
  }

  it should "handle escaped newlines in the response" in {
    val jsonResponse = """{"text": "Line 1\nLine 2"}"""
    val result       = client.extractContentFromResponse(jsonResponse)
    result shouldBe "Line 1\nLine 2"
  }

  it should "handle escaped backslash and newlines in the response" in {
    val jsonResponse = """{"text": "Line 1\\nLine 2"}"""
    val result       = client.extractContentFromResponse(jsonResponse)
    result shouldBe "Line 1\\\nLine 2"
  }

  it should "handle escaped quotes in the response" in {
    val jsonResponse = """{"text": "He said \\\"Hello\\\" to me"}"""
    val result       = client.extractContentFromResponse(jsonResponse)
    result shouldBe """He said \\\"""
  }

  it should "handle complex responses with multiple escapes" in {
    val jsonResponse = """{"text": "First line\\nSecond line with \\\"quotes\\\"\\nThird line"}"""
    val result       = client.extractContentFromResponse(jsonResponse)
    result shouldBe """First line\
Second line with \\\"""
  }

  it should "return error message when text field is not found" in {
    val jsonResponse = """{"message": "Some other content"}"""
    val result       = client.extractContentFromResponse(jsonResponse)
    result shouldBe "Could not parse response from Anthropic Vision API"
  }

  it should "handle response with whitespace around text field" in {
    val jsonResponse = """{ "text"  :   "Spaced content"   }"""
    val result       = client.extractContentFromResponse(jsonResponse)
    result shouldBe "Spaced content"
  }

  it should "handle response with no match" in {
    val jsonResponse = """{ "txt"  :   "Spaced \"content\""   }"""
    val result       = client.extractContentFromResponse(jsonResponse)
    result shouldBe "Could not parse response from Anthropic Vision API"
  }
}
