package scala.scalanative.unsafe

import scala.scalanative.unsafe
import scalanative.runtime.RawPtr
import scala.scalanative.runtime.libc.memcpy
import scalanative.unsigned.*

/** Low-level delimited continuation API.
 */
object Continuation:
  import Handler.*
  import ContinuationImpl.*

  /** Unique label for a boundary.
   */
  opaque type Label[T] = Handler.Label

  /** A captured continuation with receiving type `R` and returning type `T`. */
  opaque type Resumption[R, T] = Resume[R, T]

  private class Resume[R, T](
      val handlers: Handlers,
      val resume: ResumeData[R]
  ) {
    val oldStackTop = handlers.last._1
  }

  class ResumeData[R](
      f: Ptr[Fragment],
      _ptrToR: Ptr[R]
  ) {
    import scala.scalanative.runtime.libc
    import scala.scalanative.runtime.ObjectArray

    val stackFragment = f._1
    val fragmentSize = f._2
    val fragmentStackTop = f._3
    val ptrToROffset =
      _ptrToR.asInstanceOf[Ptr[Byte]] - f._3
    val jmpBuf: JmpBuf = CArray(libc.malloc(sizeof[JmpBuf]))

    def ptrToR(newStackTop: Ptr[Byte]) =
      (newStackTop + ptrToROffset).asInstanceOf[Ptr[R]]

    // free the fragment
    libc.free(f.rawptr)

    // mark the stack fragment as a root
    // GC.addRoots(stackFragment, stackFragment + fragmentSize)

    override def finalize() =
      // GC.removeRoots(stackFragment, stackFragment + fragmentSize)
      libc.free(stackFragment.rawptr)
      libc.free(jmpBuf.rawptr)
  }

  /** Register a handler for an action and run it */
  inline def prompt[T](action: Label[T] ?=> T)(using Tag[T]): T =
    val stackBtm = stackalloc[Byte]()
    promptFrom(stackBtm, action)

  @noinline private def promptFrom[T](stackBtm: Ptr[Byte], action: Label[T] ?=> T)(using
      tt: Tag[T]
  ): T =
    import ContinuationImpl.*
    import Handler.*
    val handler = newHandler(stackBtm)
    @volatile val label = handler._3
    println(s"old stack btm = $stackBtm, label = $label, handler = $handler")
    if setjmp(handler._4.rawptr) == 0 then
      // we set up the handler
      push_handler(handler)
      val res = action(using label)
      println(s"ending prompt, label = $label")
      pop_handler(label)
      res
    else
      println(s"looking at handler at ${get_last()}, label = ${get_last()._3}")
      !(get_last()._2.asInstanceOf[Ptr[T]])

  /** Runs `ret` and return the value `T` back to the prompt, while suspending
   *  until resumed with return value `R`.
   */
  def suspend[T, R](
      ret: Resumption[R, T] => T
  )(using l: Label[T], tt: Tag[T], tr: Tag[R]): R =
    @volatile val returnPtr = stackalloc[R]()
    val handlers = collect_up_to(l)
    val handler = handlers.last
    val resume = ResumeData(capture_to_handler(handler), returnPtr)
    val r = Resume[R, T](handlers, resume)
    !(handler._2.asInstanceOf[Ptr[T]]) = ret(r)
    val jmp = setjmp(resume.jmpBuf.rawptr)
    if jmp == 0 then
      println(s"returnPtr before = $returnPtr, suspend ptr = ${handler._2}, handler = ${handler}")
      longjmp(handler._4.rawptr, 1)
    else
      // println(s"returnPtr after = ${returnPtr.asInstanceOf[Ptr[Byte]] + jmp} (jmp = $jmp)")
      // TODO: how to find
      !(returnPtr.asInstanceOf[Ptr[Byte]] + jmp).asInstanceOf[Ptr[R]]
      // !resume.ptrToR

  /** Resumes the resumption `r`, passing it the value `value`. */
  def resume[T, R](r: Resumption[R, T], value: R)(using Tag[R]): T =
    val buf = stackalloc[JmpBuf]()
    var i = 0 // don't overwrite this!
    val btm = stackalloc[Byte]()
    val next = btm - r.resume.fragmentSize - /* safety, we might overwrite this */ 10240 // overwrite from here
    val diff = next - r.resume.fragmentStackTop
    // fix the resumption's handler offsets
    val newHandlers =
      r.handlers.map(ptr => (ptr.asInstanceOf[Ptr[Byte]] + diff).asInstanceOf[Ptr[Handler]])
    // install the handlers
    push_handler(newHandlers)
    prepareResume(diff, !buf, r)
    val postStack = r.resume.ptrToR(next)
    println(s"copying stack to ${next} ~ $btm, postStack = $postStack")
    // copy stack! don't use memcpy here because its stack will be overwritten
    while (i < r.resume.fragmentSize) do
      !(next + i) = !(r.resume.stackFragment + i)
      i += 1
    // I don't think you should do this
    // fix the handlers themselves
    newHandlers.foreach(fix_handler(_, diff))
    // set return value
    !postStack = value
    // longjmp time...
    longjmp(buf.rawptr, diff.toInt)


  def prepareResume[T, R](
      diff: CPtrDiff,
      buf: JmpBuf,
      r: Resumption[R, T]
  ) =
    // paste the jmpbuf
    memcpy(buf.rawptr, r.resume.jmpBuf.rawptr, sizeof[JmpBuf])
    // ... and fix it
    fix_jmpbuf(buf, diff)
end Continuation

private object Handler:
  import scala.collection.mutable.ListBuffer
  import ContinuationImpl.*

  type Label = CInt
  private var labelCount: Label = 0

  inline def newHandler[T](stackBtm: Ptr[Byte])(using Tag[T]) =
    val handler = stackalloc[Handler]()
    handler._1 = stackBtm
    handler._2 = stackalloc[T]().asInstanceOf[Ptr[Byte]]
    labelCount += 1
    handler._3 = labelCount
    handler

  type Handler =
    CStruct4[
      /* stack bottom */ Ptr[
        Byte
      ], /* suspended return, ALSO PART OF STACK! */ Ptr[
        Byte
      ],
      /* Label */ Label,
      ContinuationImpl.JmpBuf
    ]
  type Handlers = ListBuffer[Ptr[Handler]]
  // TODO: how to make this thread local? Or do we just move it to C..
  @volatile private val handlers: Handlers = ListBuffer.empty
  @volatile private var lastCollected: Ptr[Handler] = null

  def get_last() = lastCollected

  def push_handler(h: Ptr[Handler]) =
    handlers.addOne(h)

  def push_handler(hs: Handlers) =
    handlers.prependAll(hs)

  def collect_up_to(h: Label): Handlers =
    val handlerIdx = handlers.indexWhere(_._3 == h)
    assert(handlerIdx >= 0)
    val r = handlers.take(handlerIdx + 1)
    handlers.dropInPlace(handlerIdx)
    lastCollected = handlers.head
    handlers.dropInPlace(1)
    r

  def fix_handler(h: Ptr[Handler], diff: CPtrDiff) =
    h._1 = h._1 + diff
    h._2 = h._2 + diff
    fix_jmpbuf(h._4, diff)

  inline def fix_jmpbuf(buf: JmpBuf, diff: CPtrDiff) =
    /* probably very architecture dependent code */
    println(s"rsp change by $diff: ${buf(2)} => ${buf(2) + diff}, ${buf(3)} => ${buf(3) + diff}")
    !buf.at(2) = buf(2) + diff
    !buf.at(3) = buf(3) + diff


  def print_handlers() =
    println(s"handlers = ${handlers.map(h => s"${h} with label ${h._3}")}")

  def pop_handler(h: Label) =
    print_handlers()
    assert(handlers.head._3 == h)
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
  type _20 = Digit2[_2, _0]
  type JmpBuf = CArray[Ptr[Byte], _20]

  type Fragment = CStruct3[Ptr[Byte], CLongInt, Ptr[Byte]]
  @name("_lh_setjmp")
  @noinline def setjmp(buf: RawPtr): unsafe.CInt = extern

  @name("_lh_longjmp")
  @noinline def longjmp[T](buf: RawPtr, ret: unsafe.CInt): T = extern

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
  ): Ptr[Fragment] = extern

  // /** Restore the stack from the resumption struct */
  // @name("cont_resume")
  // def resume(resumption: Resume): Unit = extern
end ContinuationImpl
