import scala.scalanative.unsafe.Continuation.*

object Test {
  def main(args: Array[String]): Unit = {
    val p = prompt[String] {
      println("Hello, World!")
      suspend[String, Unit] { _ =>
        println("Inside suspend fn")
        "ababa"
      }
      "ababa"
    }
    println(p)
  }
}
