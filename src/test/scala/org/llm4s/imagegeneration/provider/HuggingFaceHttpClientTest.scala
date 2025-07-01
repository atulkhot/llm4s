package org.llm4s.imagegeneration.provider

import org.llm4s.imagegeneration.HuggingFaceConfig
import org.scalamock.scalatest.MockFactory
import org.scalamock.stubs.Stubs
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import requests.Response

class HuggingFaceHttpClientTest extends AnyFlatSpec with MockFactory with EitherValues {

  val httpClient: BaseHttpClient = stub[BaseHttpClient]
  val huggingFaceClient          = new HuggingFaceClient(HuggingFaceConfig("test-key", "test-model"), httpClient)

  it should "return a Left(error) on exception" in {
    val client = new HuggingFaceClient(HuggingFaceConfig("test-key", "test-model"), httpClient)
    val prompt = "A reproducible image generation"

    (httpClient.post _).when("something").throws(new RuntimeException("Something went wrong"))

    val result = huggingFaceClient.makeHttpRequest("something")

    result.isRight should be(false)
    result.left.get.message should be("Something went wrong")
  }

  it should "return a Right(value) on success" in {
    val client = new HuggingFaceClient(HuggingFaceConfig("test-key", "test-model"), httpClient)
    val prompt = "A reproducible image generation"

    val response = Response("", 200, "OK", new geny.Bytes("something".getBytes), Map.empty, None)
    (httpClient.post _).when("something").returns(response)

    val result = huggingFaceClient.makeHttpRequest("something")

    result.isRight should be(true)
    result.value should be(response)
  }

}
