package org.llm4s.trace

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EnhancedTracingSettingsSpec extends AnyWordSpec with Matchers {

  private def isLangfuseConfigured: Boolean =
    Option(System.getenv("LANGFUSE_PUBLIC_KEY")).exists(_.nonEmpty) ||
      Option(System.getenv("TRACING_MODE")).exists(_.equalsIgnoreCase("langfuse")) ||
      Option(System.getProperty("llm4s.tracing.mode")).exists(_.equalsIgnoreCase("langfuse"))

  "EnhancedTracing.createFromEnv" should {
    "return Console tracing by default" in {
      // Skip test if Langfuse is configured in environment
      if (isLangfuseConfigured) {
        cancel("Test skipped: Langfuse is configured in environment (LANGFUSE_PUBLIC_KEY or TRACING_MODE=langfuse)")
      }

      val res = EnhancedTracing.createFromEnv()
      res.fold(err => fail(err.toString), tracer => tracer shouldBe a[EnhancedConsoleTracing])
    }
  }
}
