package org.llm4s.speech.processing

import org.llm4s.speech.AudioMeta
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class STTMetadataValidatorSpec extends AnyFlatSpec with Matchers {

  // Helper method to create AudioMeta for testing
  private def createAudioMeta(
                               sampleRate: Int = 16000,
                               numChannels: Int = 1,
                               bitDepth: Int = 16
                             ): AudioMeta = AudioMeta(
    sampleRate = sampleRate,
    numChannels = numChannels,
    bitDepth = bitDepth
  )

  "validateSampleRate" should "validate positive sample rates" in {
    val validator = new AudioValidator.STTMetadataValidator()

    // Valid sample rates
    val validSampleRates = List(8000, 16000, 22050, 44100, 48000)
    validSampleRates.foreach { rate =>
      val meta = createAudioMeta(sampleRate = rate)
      val result = validator.validate(meta)
      result.isRight shouldBe true
    }
  }

  it should "reject zero or negative sample rates" in {
    val validator = new AudioValidator.STTMetadataValidator()

    // Invalid sample rates
    val invalidSampleRates = List(0, -1, -16000)
    invalidSampleRates.foreach { rate =>
      val meta = createAudioMeta(sampleRate = rate)
      val result = validator.validate(meta)
      result match {
        case Left(error) =>
          error.message should include("Sample rate must be positive")
          error.message should include(s"sampleRate = $rate")
        case Right(_) => fail(s"Expected validation failure for sample rate $rate")
      }
    }
  }

  it should "reject sample rates above 48000" in {
    val validator = new AudioValidator.STTMetadataValidator()

    // Invalid high sample rates
    val invalidSampleRates = List(48001, 96000, 192000)
    invalidSampleRates.foreach { rate =>
      val meta = createAudioMeta(sampleRate = rate)
      val result = validator.validate(meta)
      result match {
        case Left(error) =>
          error.message should include("Sample rate too high for STT")
          error.message should include(s"Sample rate = $rate")
        case Right(_) => fail(s"Expected validation failure for sample rate $rate")
      }
    }
  }

  "validateNumChannels" should "validate positive number of channels" in {
    val validator = new AudioValidator.STTMetadataValidator()

    // Valid channel counts
    val validChannelCounts = List(1, 2)
    validChannelCounts.foreach { channels =>
      val meta = createAudioMeta(numChannels = channels)
      val result = validator.validate(meta)
      result.isRight shouldBe true
    }
  }

  it should "reject zero or negative channel counts" in {
    val validator = new AudioValidator.STTMetadataValidator()

    // Invalid channel counts
    val invalidChannelCounts = List(0, -1, -2)
    invalidChannelCounts.foreach { channels =>
      val meta = createAudioMeta(numChannels = channels)
      val result = validator.validate(meta)
      result match {
        case Left(error) =>
          error.message should include("Number of channels must be positive")
          error.message should include(s"Number of channels = $channels")
        case Right(_) => fail(s"Expected validation failure for channel count $channels")
      }
    }
  }

  "validateBitDepth" should "validate 16-bit audio" in {
    val validator = new AudioValidator.STTMetadataValidator()

    val meta = createAudioMeta(bitDepth = 16)
    val result = validator.validate(meta)
    result.isRight shouldBe true
  }

  it should "reject non-16-bit audio" in {
    val validator = new AudioValidator.STTMetadataValidator()

    // Invalid bit depths
    val invalidBitDepths = List(8, 24, 32, 64)
    invalidBitDepths.foreach { bitDepth =>
      val meta = createAudioMeta(bitDepth = bitDepth)
      val result = validator.validate(meta)
      result match {
        case Left(error) =>
          error.message should include("Only 16-bit audio is supported")
          error.message should include(s"Bit depth = $bitDepth")
        case Right(_) => fail(s"Expected validation failure for bit depth $bitDepth")
      }
    }
  }

  "validateMeta" should "accumulate multiple validation errors" in {
    val validator = new AudioValidator.STTMetadataValidator()

    val invalidMeta = createAudioMeta(
      sampleRate = 0,      // Invalid sample rate
      numChannels = -1,    // Invalid channels
      bitDepth = 24        // Invalid bit depth
    )

    val result = validator.validate(invalidMeta)
    result match {
      case Left(error) =>
        error.message should include("Sample rate must be positive")
        error.message should include("Number of channels must be positive")
        error.message should include("Only 16-bit audio is supported")
      case Right(_) => fail("Expected multiple validation errors")
    }
  }

  "validate method" should "handle non-AudioMeta input" in {
    val validator = new AudioValidator.STTMetadataValidator()

    val invalidInput = "Not an AudioMeta"
    val result = validator.validate(invalidInput)

    result match {
      case Left(error) =>
        error.message should include("Input must be AudioMeta")
      case Right(_) => fail("Expected validation failure for non-AudioMeta input")
    }
  }
}