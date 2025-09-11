package org.llm4s.speech.processing

import cats.data._
import cats.implicits._
import org.llm4s.speech.AudioMeta
import org.llm4s.types.{NewValidatedResult, Result}
import org.llm4s.error.{ProcessingError, ValidationErrors}

/**
 * Generic audio validator trait for validating audio data and metadata.
 * This provides a more flexible and extensible design for audio validation.
 * Made covariant in A as suggested in the review.
 */
trait AudioValidator[+A] {
  def validate[B >: A](input: B): Result[B]
  def name: String
}

/**
 * Cats Validated-based audio validator for better error accumulation.
 * This accumulates all validation errors instead of failing fast.
 */
trait ValidatedAudioValidator[A] {
  def validate(input: A): ValidatedNec[ProcessingError, A]
  def name: String

  /** Convert ValidatedNel to Result for compatibility */
  def validateAsResult(input: A): Result[A] = validate(input).toEither.left.map(_.head)
}

/**
 * Audio validation implementations
 */
object AudioValidator {
  private type ValidationResult[A] = ValidatedNec[String, A]

  implicit class AudioValidatorOps[A](val validator: ValidationResult[A]) extends AnyVal {
    def toLLMValidatedResult: NewValidatedResult[A] =
      validator.toEither
  }

  private def validateSampleRate(meta: AudioMeta): ValidationResult[AudioMeta] =
    if (meta.sampleRate <= 0)
      s"sampleRate = ${meta.sampleRate}, Sample rate must be positive".invalidNec
    else
      meta.validNec

  private def validateNumChannels(meta: AudioMeta): ValidationResult[AudioMeta] =
    if (meta.numChannels <= 0)
      s"Number of channels = ${meta.numChannels}, Number of channels must be positive".invalidNec
    else
      meta.validNec

  private def validateBitDepth(meta: AudioMeta): ValidationResult[AudioMeta] =
    if (meta.bitDepth != 16)
      s"Bit depth = ${meta.bitDepth}, Only 16-bit audio is supported".invalidNec
    else
      meta.validNec

  private def validateSampleRateRange(meta: AudioMeta): ValidationResult[AudioMeta] =
    if (meta.sampleRate > 48000)
      s"Sample rate = ${meta.sampleRate}, Sample rate too high for STT".invalidNec
    else
      meta.validNec

  /**
   * Validates audio metadata for STT processing
   */
  case class STTMetadataValidator() extends AudioValidator[AudioMeta] {
    def validate[B >: AudioMeta](input: B): Result[B] = input match {
      case meta: AudioMeta => validateMeta(meta).leftMap(nec => ValidationErrors(nec))
      case _               => Left(ProcessingError.audioValidation("Input must be AudioMeta"))
    }

    private def validateMeta(meta: AudioMeta): NewValidatedResult[AudioMeta] = {
      val result: ValidationResult[AudioMeta] =
        (validateSampleRate(meta), validateNumChannels(meta), validateBitDepth(meta), validateSampleRateRange(meta))
          .mapN((_, _, _, _) => meta)
      result.toLLMValidatedResult
    }

    def name: String = "stt-metadata-validator"
  }

  /**
   * Validates audio data length matches metadata
   */
  case class AudioDataValidator() extends AudioValidator[(Array[Byte], AudioMeta)] {
    def validate[B >: (Array[Byte], AudioMeta)](input: B): Result[B] = input match {
      case data: (Array[Byte], AudioMeta) @unchecked => validateData(data).asInstanceOf[Result[B]]
      case _ => Left(ProcessingError.audioValidation("Input must be (Array[Byte], AudioMeta)"))
    }

    private def validateData(input: (Array[Byte], AudioMeta)): Result[(Array[Byte], AudioMeta)] = {
      val (bytes, meta)  = input
      val expectedLength = meta.numChannels * (meta.bitDepth / 8)

      if (bytes.length % expectedLength != 0) {
        Left(
          ProcessingError.audioValidation(
            s"Audio data length (${bytes.length}) is not a multiple of frame size (${expectedLength})"
          )
        )
      } else {
        Right(input)
      }
    }

    def name: String = "audio-data-validator"
  }

  /**
   * Validates audio is not empty
   */
  case class NonEmptyAudioValidator() extends AudioValidator[(Array[Byte], AudioMeta)] {
    def validate[B >: (Array[Byte], AudioMeta)](input: B): Result[B] = input match {
      case data: (Array[Byte], AudioMeta) @unchecked => validateNonEmpty(data).asInstanceOf[Result[B]]
      case _ => Left(ProcessingError.audioValidation("Input must be (Array[Byte], AudioMeta)"))
    }

    private def validateNonEmpty(input: (Array[Byte], AudioMeta)): Result[(Array[Byte], AudioMeta)] = {
      val (bytes, _) = input
      if (bytes.isEmpty) {
        Left(ProcessingError.audioValidation("Audio data is empty"))
      } else {
        Right(input)
      }
    }

    def name: String = "non-empty-audio-validator"
  }

  /**
   * Composes multiple validators
   */
  case class CompositeValidator[A](
    validators: List[AudioValidator[A]]
  ) extends AudioValidator[A] {

    def validate[B >: A](input: B): Result[B] =
      validators.foldLeft(Right(input): Result[B])((acc, validator) => acc.flatMap(validator.validate))

    def name: String = validators.map(_.name).mkString(" + ")
  }

  /**
   * Standard STT validation pipeline
   */
  def sttValidator: AudioValidator[(Array[Byte], AudioMeta)] =
    CompositeValidator(
      List(
        NonEmptyAudioValidator(),
        AudioDataValidator()
      )
    )

  // ===== Cats Validated implementations =====

  /**
   * Validated version of NonEmptyAudioValidator using Cats Validated
   */
  case class ValidatedNonEmptyAudioValidator() extends ValidatedAudioValidator[(Array[Byte], AudioMeta)] {
    def validate(
      input: (Array[Byte], AudioMeta)
    ): ValidatedNec[ProcessingError, (Array[Byte], AudioMeta)] = {
      val (bytes, _) = input
      if (bytes.isEmpty) {
        Validated.invalidNec(ProcessingError.audioValidation("Audio data is empty"))
      } else {
        Validated.valid(input)
      }
    }

    def name: String = "validated-non-empty-audio-validator"
  }

  /**
   * Validated version of AudioDataValidator using Cats Validated
   */
  case class ValidatedAudioDataValidator() extends ValidatedAudioValidator[(Array[Byte], AudioMeta)] {
    def validate(
      input: (Array[Byte], AudioMeta)
    ): ValidatedNec[ProcessingError, (Array[Byte], AudioMeta)] = {
      val (bytes, meta)  = input
      val expectedLength = meta.numChannels * (meta.bitDepth / 8)

      if (bytes.length % expectedLength != 0) {
        Validated.invalidNec(
          ProcessingError.audioValidation(
            s"Audio data length (${bytes.length}) is not a multiple of frame size (${expectedLength})"
          )
        )
      } else {
        Validated.valid(input)
      }
    }

    def name: String = "validated-audio-data-validator"
  }

  /**
   * Validated version of STTMetadataValidator using Cats Validated
   */
  case class ValidatedSTTMetadataValidator() extends ValidatedAudioValidator[AudioMeta] {

    def validate(meta: AudioMeta): ValidatedNec[ProcessingError, AudioMeta] = {
      (validateSampleRate(meta), validateNumChannels(meta), validateBitDepth(meta), validateSampleRateRange(meta))
        .mapN((_, _, _, _) => meta).leftMap(nec => nec.map(_ => ProcessingError.audioValidation(
          nec.toNonEmptyList.toList.mkString(","))
        ))
    }

    def name: String = "validated-stt-metadata-validator"
  }

  /**
   * Validated STT validation pipeline using Cats Validated mapN
   * This demonstrates the improved composition requested in the review
   */
  def validatedSttValidator(
    input: (Array[Byte], AudioMeta)
  ): ValidatedNec[ProcessingError, (Array[Byte], AudioMeta)] = {
    val nonEmptyValidator = ValidatedNonEmptyAudioValidator()
    val dataValidator     = ValidatedAudioDataValidator()

    // Use mapN to combine validators - accumulates all errors instead of failing fast
    (nonEmptyValidator.validate(input), dataValidator.validate(input)).mapN((_, validated) => validated)
  }

  /**
   * Convert the validated STT validator result to Result for existing API compatibility
   */
  def validatedSttValidatorAsResult(input: (Array[Byte], AudioMeta)): Result[(Array[Byte], AudioMeta)] =
    validatedSttValidator(input).toEither.left.map(_.head)
}
