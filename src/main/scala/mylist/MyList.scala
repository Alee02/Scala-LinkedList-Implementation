package mylist

abstract class MyList[+A] {
  def head: A
  def isEmpty: Boolean
  def tail: MyList[A]
  def add[B >: A](element: B): MyList[B]
  def printElements: String
  override def toString: String = {
    "MyList("+  printElements + ")"
  }

  def map[B](transformer: Function1[A, B]): MyList[B]
//  def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B]
  def filter(predicate: MyPredicate[A]): MyList[A]
}

class Empty[A]

object Empty extends MyList {

  override def head: Nothing = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def tail: MyList[Nothing] = throw new NoSuchElementException

  override def add[B >: Nothing](element: B): MyList[B] = new Cons(element, Empty)

  override def printElements: String = ""

  override def map[B](transformer: Function1[Nothing, B]): MyList[B] = Empty

//  override def flatMap[B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = Empty

  override def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = Empty
}

class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  def head: A = h
  def isEmpty: Boolean = false
  def tail: MyList[A] = t
  def add[B >: A](element: B): MyList[B] = new Cons(element, this)

  def printElements: String = {
    if(t.isEmpty) "" + h
    else h + ", " + t.printElements
  }

  override def map[B](transformer: Function1[A, B]): MyList[B] = {
    new Cons[B](transformer(h), t.map(transformer))
  }

//  override def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] = ???

  override def filter(predicate: MyPredicate[A]): MyList[A] = {
    if (predicate.test(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)
  }
}

trait MyPredicate[-T] {
  def test(element: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(element: A): B
}

object ListTest extends App {

  val list = new Cons(true, new Cons(true , new Cons(true, Empty)))
  val listOfIntegers = new Cons(1, new Cons(2 , new Cons(3, Empty)))
  println(listOfIntegers.map(a => a * 2))
}