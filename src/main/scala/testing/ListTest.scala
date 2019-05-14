package testing

import mylist._

object ListTest extends App {
  val list = new Cons(true, new Cons(true , new Cons(true, Empty)))
  val listOfIntegers = new Cons(1, new Cons(2 , new Cons(3, Empty)))

  val listOfIntegers2 = new Cons(1, new Cons(2 , new Cons(3, Empty)))

  println(listOfIntegers.map(_ * 2))

}
