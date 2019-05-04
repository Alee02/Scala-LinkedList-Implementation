package testing

object Generics extends App{
  // lists should be Covariant
  class MyList[+A] {
    // use the type A

    def add[B >: A](element: B): MyList[B] = ???
    /*
    Explanation: If an Element of Super Type B is added to a MyList[A]
     then it should return a MyList[B] super Type to make the list more general.
     */

  }

  class MyMap[Key, Value]
  val listOfIntegers = new MyList[Int]
  val listOfStrings = new MyList[String]

  object MyList{
    def empty[A]: MyList[A] = ???
  }

  val emptyListOfIntegers = MyList.empty[Int]

  // variance problem
  class Animal
  class Cat extends Animal
  class Dog extends Animal

  //1. yes List[Cat] extends List[Animal] = COVARIANCE
  class CovariantList[+A]

  val animal: Animal = new Cat
  val animalList: CovariantList[Animal] = new CovariantList[Cat]

  // animalList.add(new Dog) ??? HARD QUESTION.

  //2. NO List[Cat] and List[Animal] are two different things => INVARIANCE

  class InvariantList[A]
  // below will not compiile.
//  val invariantAnimalList: InvariantList[Animal] = new InvariantList[Cat]
  val invariantAnimalList: InvariantList[Animal] = new InvariantList[Animal]

  // 3. Hell, No! CONTRAVARIANCE
  // (Opposite relationship .. doesn't really make sense) Does feel very very intuitive
  // Atleast for Lists
  class ContravariantList[-A]
  val contravariantList: ContravariantList[Cat] = new ContravariantList[Animal]

  class Trainer[-A]
  // In this case Trainer[Animal] is better than a Trainer[Cat]
  // It makes more sense and is more intuitive.
  val trainer: Trainer[Cat] = new Trainer[Animal]

  // bounded Types
  class Cage[A <: Animal](animal: A)

  val cage = new Cage[Dog](new Dog)

  class Car
  // This will throw an error because cage only accepts type that is a a sub type of animal
//  val newCage = new Cage(new Car)






}
