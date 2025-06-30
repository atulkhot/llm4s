package org.llm4s.imagegeneration.provider

class HuggingFaceHttpClient(config: HuggingFaceConfig) {
  private val logger = org.slf4j.LoggerFactory.getLogger(getClass)

  def makeRequest(payload: String): Either[ImageGenerationError, requests.Response] = {
    val url = s"https://api-inference.huggingface.co/models/${config.model}"
    val headers = Map(
      "Authorization" -> s"Bearer ${config.apiKey}",
      "Content-Type" -> "application/json"
    )

    logger.debug("Making request to: {}", url)
    logger.debug("Payload: {}", payload)

    val result = Try {
      requests.post( // Note that the post could throw - as per the documentation
        url = url,
        data = payload,
        headers = headers,
        readTimeout = config.timeout,
        connectTimeout = 10000
      )
    }.toEither.left.map(exception => ServiceError(exception.getMessage, 500))

    result.flatMap { response =>
      if (response.statusCode == 200) {
        Right(response)
      } else {
        Left(ServiceError(response.text(), 500))
      }
    }
  }
}
