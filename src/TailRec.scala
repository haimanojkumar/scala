import scala.annotation.tailrec

/**
  * Created by manojmohan on 1/9/16.
  */
object TailRec {

  //Recursion
  def factorial(n: Int): Int = {
    if (n == 0) 1
    else n * factorial(n - 1)
  }

  //Tail recursion
  //https://en.wikipedia.org/wiki/Tail_call
  def factorTail(n: Int): Int = {
    @tailrec def fact(n: Int, acc: Int): Int = {
      n match {
        case 0 => acc
        case _ => fact(n-1, acc * n)
      }

    }
    fact(n, 1)
  }

  def main(args: Array[String]) {
    println("factorial of 6 is " + factorial(6))
    println("factorial of 6 is " + factorTail(6))
  }
}
