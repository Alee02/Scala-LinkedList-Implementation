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
}

class Empty[A]

object Empty extends MyList {
//  def head:  =
//  def isEmpty: Boolean = true
//  def tail: MyList = throw new NoSuchElementException
//  def add(element: Int): MyList = new Cons(element, Empty)
//
//  def printElements: String = ""
  override def head: Nothing = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def tail: MyList[Nothing] = throw new NoSuchElementException

  override def add[B >: Nothing](element: B): MyList[B] = new Cons(element, Empty)

  override def printElements: String = ""
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
}

object ListTest extends App {

  val list = new Cons(true, new Cons(true , new Cons(true, Empty)))
  println(list)
}