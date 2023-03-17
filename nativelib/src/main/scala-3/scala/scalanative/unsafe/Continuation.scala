package scala.scalanative.unsafe

import scala.scalanative.unsafe
import scalanative.runtime.RawPtr

/** Low-level delimited continuation API.
 */
object Continuation:
  import Handler.*
  import ContinuationImpl.*

  /** Unique label for a boundary.
   */
  opaque type Label[T] = Ptr[Handler]

  /** A captured continuation with receiving type `R` and returning type `T`. */
  opaque type Resumption[R, T] = Resume[R, T]

  private class Resume[R, T](
      val handlers: Handlers,
      val resume: ResumeData
  )

  class ResumeData(
      f: Fragment
  ) {
    import scala.scalanative.runtime.libc
    import scala.scalanative.runtime.ObjectArray

    val stackFragment = f._1
    val fragmentSize = f._2
    val jmpBuf: Ptr[JmpBuf] = Ptr(libc.malloc(sizeof[JmpBuf]))

    // mark the stack fragment as a root
    // GC.addRoots(stackFragment, stackFragment + fragmentSize)

    override def finalize() =
      // GC.removeRoots(stackFragment, stackFragment + fragmentSize)
      libc.free(stackFragment.rawptr)
      libc.free(jmpBuf.rawptr)
  }

  /** Register a handler for an action and run it */
  def prompt[T](action: Label[T] ?=> T)(using Tag[T]): T =
    val stackTop = stackalloc[Byte]()
    promptFrom(stackTop + 1, action)

  @noinline private def promptFrom[T](
      stackTop: Ptr[Byte],
      action: Label[T] ?=> T
  )(using tt: Tag[T]): T =
    import ContinuationImpl.*
    import Handler.*
    val handler = stackalloc[Handler]()
    handler._1 = stackTop
    handler._2 = 0.toPtr
    if setjmp(handler.at3.rawptr) == 0 then
      // we set up the handler
      push_handler(handler)
      val res = action(using handler)
      pop_handler(handler)
      res
    else (!handler._2).asInstanceOf[T]

    // val handler = save_handler()
    // println("here")
    // if handler == 0.toPtr then
    //   // we are back from the handler
    //   // (!handler._3).asInstanceOf[T]
    //   ???
    // else
    //   // we just set up the handler
    //   push_handler(handler)
    //   val res = action(using handler)
    //   pop_handler(handler)
    //   res

  /** Runs `ret` and return the value `T` back to the prompt, while suspending
   *  until resumed with return value `R`.
   */
  def suspend[T, R](ret: Resumption[R, T] => T)(using l: Label[T]): R =
    // TODO: setjmp somewhere here
    val handlers = collect_up_to(l)
    val resume =
      val fragment = capture_to_handler(l)
      ResumeData(fragment)
    val r = Resume[R, T](handlers, resume)
    !(l._2) = ret(r).asInstanceOf[Object]
    longjmp(l.at3.rawptr, 1)

  /** Resumes the resumption `r`, passing it the value `value`. */
  def resume[T, R](r: Resumption[R, T], value: R): T = ???

end Continuation

private object Handler:
  import scala.collection.mutable.ListBuffer

  type Handler =
    CStruct3[ /* stack top */ Ptr[Byte], /* suspended return */ Ptr[
        Object
      ], ContinuationImpl.JmpBuf]
  type Handlers = ListBuffer[Ptr[Handler]]
  // TODO: how to make this thread local? Or do we just move it to C..
  private val handlers: Handlers = ListBuffer.empty

  def push_handler(h: Ptr[Handler]) =
    handlers.addOne(h)

  def collect_up_to(h: Ptr[Handler]): Handlers =
    val r = handlers.takeWhileInPlace(_ != h)
    r.append(h)
    handlers.dropInPlace(1)
    r

  def pop_handler(h: Ptr[Handler]) =
    assert(handlers.head == h)
    handlers.dropInPlace(1)
end Handler

/** Low(er?)-level C implementations of handlers and resumptions.
 */
@extern
private object ContinuationImpl:
  // /** Pushes a new handler frame with the current stack top. */
  // @name("cont_push_handler_frame")
  // def push_handler_frame(): RawPtr = extern
  // /** Pops the current frame off the handler stack. */
  // @name("cont_pop_handler_frame")
  // def pop_handler_frame(handler: RawPtr): Unit = extern

  import scalanative.unsafe._, Nat._

  /** Very generous memory buffer for a jump buffer */
  type JmpBuf = CArray[CLongLong, Digit2[_1, _6]]

  type Fragment = CStruct2[Ptr[Byte], CUnsignedLongInt]

  // @name("_setjmp")
  def setjmp(buf: RawPtr): unsafe.CInt = extern

  // @name("longjmp")
  def longjmp[T](buf: RawPtr, ret: unsafe.CInt): T = extern

  // @name("cont_save_handler")
  // /** Saves the handler state. Returns the handler struct. */
  // def save_handler(): Ptr[Handler.Handler] = extern

  // /** Flies back to the handler. */
  // def resume_to_handler[T](h: Ptr[Handler.Handler]): T = extern

  /** Capture the stack up to the given handler, and return the captured
   *  fragment.
   */
  @name("cont_capture_to_handler")
  def capture_to_handler(
      handler: Ptr[Handler.Handler]
  ): Fragment = extern

  // /** Restore the stack from the resumption struct */
  // @name("cont_resume")
  // def resume(resumption: Resume): Unit = extern
end ContinuationImpl
