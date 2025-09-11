package org.llm4s.error

import cats.data.NonEmptyChainOps

case class ValidationErrors(nec: NonEmptyChainOps[String]) extends LLMError {
  /** Human-readable error message */
  override def message: String =
    nec.toNonEmptyList.toList.mkString(", ")
}
