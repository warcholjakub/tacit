package tacit.library

import language.experimental.captureChecking
import caps.unsafe.unsafeAssumePure
import caps.*

import scala.util.{Failure, Success, Try}

private[library] final class ClassifiedImpl[+T](private val value: Try[T]) extends Classified[T]:
  def map[B](op: T ->{any.rd} B): Classified[B] =
    ClassifiedImpl(value.flatMap(v => ClassifiedImpl.attempt(op(v))).unsafeAssumePure)
  def flatMap[B](op: T ->{any.rd} Classified[B]): Classified[B] =
    ClassifiedImpl(value.flatMap(v => ClassifiedImpl.attempt(op(v)).flatMap(ClassifiedImpl.unwrap)).unsafeAssumePure)
  override def toString: String = "Classified(***)"

private[library] object ClassifiedImpl:
  def wrap[T](value: ->{any.rd} T): Classified[T] = ClassifiedImpl(attempt(value).unsafeAssumePure)
  /** Wrap an already-evaluated result. Used by impure library sinks (e.g. an
   *  HTTP POST of classified data) that must run an effectful computation and
   *  then re-classify its outcome. The effect happens *before* wrapping, so it
   *  cannot go through `wrap`'s pure-function parameter. */
  def fromTry[T](value: Try[T]): Classified[T] = ClassifiedImpl(value.unsafeAssumePure)
  def unwrap[T](c: Classified[T]): Try[T] = c.asInstanceOf[ClassifiedImpl[T]].value

  /** Like `Try`, but keeps *every* throwable inside, fatal ones included
   *  (InterruptedException, StackOverflowError, ...). `Try` rethrows those, and
   *  one that escaped `map` would hand agent code the classified value in its
   *  message, or a bit of it by whether it was thrown. Swallowing an interrupt
   *  is harmless here: a timed-out evaluation is abandoned anyway. */
  private def attempt[T](value: => T): Try[T] =
    try Success(value)
    catch case e: Throwable => Failure(e)
