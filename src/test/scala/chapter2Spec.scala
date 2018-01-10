import org.scalatest.{FlatSpec, FunSuite, Matchers}
import MyPackage.chapter2

import scala.collection.mutable

class chapter2 extends FunSuite {
  test("the name is set correctly in constructor") {
    val name = "Evren"
    assert(name == "Evren")
  }

  test("Returns absolute value of an integer"){
      assert(25 == chapter2.abs(-25))
  }

  test("factorial returns 24 for 4"){
      assert(chapter2.factorial(4) == 24)
  }

  test("Findfirst finds 2 for index"){
    val arr = Array(1, 2, 3, 4, 5)

    assert(chapter2.findFirst(arr, 3) == 2)
  }

  test("Generic findFirst returns 2 for string array"){
    val arr = Array("a", "b", "c", "d")

    assert(chapter2.findFirstGen(arr, "c") == 2)
  }

  test("Generic findFirst returns 2 for int array"){
    val arr = Array(1, 2, 3, 4, 5)

    assert(chapter2.findFirstGen(arr, 3) == 2)
  }
}

class OtherTests extends FlatSpec with Matchers {
  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new mutable.Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be (2)
    stack.pop() should be (1)
  }


  "findFirstHof" should "return 2 for index" in {
    val arr = Array(1,2,3,4,5,6)
    val f = (x: Int) => x == 3

    chapter2.findFirstHof(arr, f) should be (2)
  }
}