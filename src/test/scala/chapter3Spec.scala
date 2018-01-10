package fpinscala
import org.scalatest.FunSuite

class chapter3 extends FunSuite {

  test("tail removes first item from List"){
    val list = List(1,2,3)

    assert(List.tail(list) == List(2, 3))
  }

  test("drop removes first n elements form list"){
    val list = List(1,2,3,4)

    assert(List.drop(list, 3) == List(4))
  }

  test("drop elements until predicate met"){
      val list = List(1,2,3,4)

      assert(List.dropWhile(list)(x=> x==3) == List(3,4))
  }

  test("setHead replaces the head of list"){
    val list = List(1,2,3,4)

    assert(List.setHead(56, list) == List(56, 2,3,4))
  }
}