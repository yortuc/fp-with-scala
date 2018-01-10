package MyPackage
import scala.annotation.tailrec

object chapter2 {

  def abs(n: Int): Int =
    if(n < 0) -n
    else n

  def formatAbs(n: Int): String = {
    val msg = s"Absolute value of %d is %d"
    msg.format(n, abs(n))
  }

  def factorial(n: Int) : Int = {
    @tailrec
    def go(n: Int, acc: Int) : Int = {
      if(n <= 0) acc
      else go(n-1, n*acc)
    }

    go(n, 1)
  }

  def findFirst(arr: Array[Int], n: Int): Int = {
    def go(i: Int) : Int ={
      if(i == arr.length) -1
      else if(arr(i) == n) i
      else go(i+1)
    }

    go(0)
  }

  def findFirstGen[A](arr: Array[A], n: A) : Int = {
    def go(i: Int): Int = {
      if(i == arr.length) -1
      else if (arr(i) == n) i
      else go(i+1)
    }

    go(0)
  }

  def findFirstHof[A](arr: Array[A], f: A => Boolean) : Int = {
    def go(i: Int): Int = {
      if(i == arr.length) -1
      else if(f(arr(i))) i
      else go(i+1)
    }
    go(0)
  }

  def formatResult(name: String, f: Int => Int, n: Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }


  def double(a: Int) = 2 * a

  // PARTIAL APPLICATION EX:3

  def partial1[A, B, C](a: A, f: (A, B)=> C): B => C = {
    def ret(p: B): C = f(a, p)
    ret
  }

  def sumTwoNums(a: Int, b: Double): String = "sum is %d".format(a+b.toInt)

  def partializedSumTwoNumsWith5 = partial1(5, sumTwoNums)
  def partializedSumTwoNumsWith7 = partial1(7, sumTwoNums)


  // CURRYING EX:4
  //
  // f(a, b) => c
  // f(a) => f(b) => f(c)
  //

  def curry[A,B,C](f: (A, B)=> C): A => (B => C) = {
    def ret(a: A) : B => C = f(a, _)
    ret
  }

  def multip(a: Int, b: Int): Int = a * b

  def multip5 = curry(multip)(5)
  def multip12 = curry(multip)(12)


  // UNCURRYING EX:5
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    def ret(a:A ,b:B): C = f(a)(b)
    ret
  }

  def unCurriedMultip = uncurry(curry(multip))

  // FUNCTION COMPOSITION EX:6
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    def ret(a: A): C = f(g(a))
    ret
  }

  def aToB(a: Int): Int = a * 2
  def bToC(b: Int): Int = b * 4
  val aToC = compose(bToC, aToB)

  // Scala Standart lib func composition
  val f = (x: Double) => math.Pi / 2 - x
  val cos = f andThen math.sin

  def main(args: Array[String]): Unit = {

    println(formatResult("absolute value", abs, -5))
    println(formatResult("square", (x:Int)=> x*x, 6))
    println(formatResult("double", double, 8))
    print(formatResult("increment 1", _ + 1, 7))

    println(partializedSumTwoNumsWith5(12.0))
    println(partializedSumTwoNumsWith7(11.0))

    println(multip5(2))
    println(multip12(4))

    println(unCurriedMultip(4, 5))
    println(aToC(7))

    println(cos(0.5))
  }
}
