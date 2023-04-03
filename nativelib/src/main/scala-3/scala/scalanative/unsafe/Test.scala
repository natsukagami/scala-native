package scala.scalanative.unsafe

import scala.scalanative.unsafe.*

object Test:
  import Nat.*
  type _20 = Digit2[_2, _0]
  type JmpBuf = CArray[Ptr[Byte], _20]

  def mkBuf = stackalloc[JmpBuf]()
