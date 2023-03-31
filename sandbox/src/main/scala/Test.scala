import scala.scalanative.unsafe.Continuation.*

object Test {
  def main(args: Array[String]): Unit = {

    enum Feed[T, R]:
      case Continue(val cur: T, val feed: R => Feed[T, R])

    import Feed.*
    type FInt = Feed[Int, Unit]

    def fib() =
      prompt[FInt] {
        var a = 1
        var b = 1
        while true do 
          suspend[FInt, Unit] { resumption => {
            println(s"resuming with $a")
            Continue(a, r => resume(resumption, r))
          } }
          val c = a + b
          println(s"fib $a $b $c")
          a = b
          b = c
          println(s"fib $a $b $c")
        ???
      }

    var f = fib()
    for (i <- 1 to 10) {
      f match {
        case Continue(v, next) => {
          println(s"$i = $v")
          f = next(())
        }
      }
    }
  }
}
