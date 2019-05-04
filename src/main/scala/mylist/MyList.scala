package mylist

abstract class MyList {
  /*
  head = first element of the list
  tail = reminder of the list
  isEmpty = is this list empty
  add(int) => new list with this element added
  toString => a string representation of the list
   */
  def head: Int
  def isEmpty: Boolean
  def tail: MyList
  def add(element: Int): MyList
  def printElements: String
  override def toString: String = {
    "MyList("+  printElements + ")"
  }
}

object Empty extends MyList {
  def head: Int = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def tail: MyList = throw new NoSuchElementException
  def add(element: Int): MyList = new Cons(element, Empty)

  def printElements: String = ""
}

class Cons(h: Int, t: MyList) extends MyList {
  def head: Int = h
  def isEmpty: Boolean = false
  def tail: MyList = t
  def add(element: Int): MyList = new Cons(element, this)

  def printElements: String = {
    if(t.isEmpty) "" + h
    else h + ", " + t.printElements
  }
}

object ListTest extends App {
  val list = new Cons(1, new Cons(2 , new Cons(3, Empty)))
  println(list.toString)
}