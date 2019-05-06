package testing

import mylist._

object ListTest extends App {
  val list = new Cons(true, new Cons(true , new Cons(true, Empty)))
  val listOfIntegers = new Cons(1, new Cons(2 , new Cons(3, Empty)))

  val listOfIntegers2 = new Cons(1, new Cons(2 , new Cons(3, Empty)))

  println(listOfIntegers.filter(new MyPredicate[Int] {
    override def test(element: Int): Boolean = element < 2
  }))

  println(listOfIntegers ++ listOfIntegers2)

  // flatMap
  println(listOfIntegers.flatMap(new MyTransformer[Int, MyList[Int]] {
    override def transform(element: Int): MyList[Int] = {
      new Cons(element, new Cons(element + 1, Empty))
    }
  }))

}
