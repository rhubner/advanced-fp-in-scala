package lambdaconf.typeclasses

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

object exercise1 {


  trait Debug[A] {

    def show(a: A): String

    def read(s: String): Either[String, A]



  }

  //  read(show(a)) == Right(a)
  //  show(s) match {
  //    case Left(_) => true
  //    case Right(a) => show(a) == s
  //  }

  object Debug {
    def apply[A](implicit v : Debug[A]): Debug[A] = v

    implicit val IntDebug: Debug[Int] = new Debug[Int] {

      import scala.util.Try

      def show(a: Int): String = a.toString

      def read(s: String): Either[String, Int] =  Try(s.toInt).map(Right(_)).getOrElse(Left("The value is not int"))

    }

  }


  implicit class DebugShowSyntax[A](self: A)(implicit debug: Debug[A]) {
    def showIt : String = debug.show(self)
  }

  implicit class DebugReadSyntax(self: String) {
    def readIt[A: Debug]: Either[String, A] = Debug[A].read(self)
  }

  1.showIt
  new DebugShowSyntax(1)(Debug.IntDebug).showIt
  "1".readIt


  case class Person(name : String)
  object Person {
    implicit val PersonDebug: Debug[Person] = new Debug[Person] {
      override def show(a: Person): String = ???

      override def read(s: String): Either[String, Person] = ???
    }
  }

  Debug[Person].show(Person("aaa"))

  Person("aaa").showIt


  // Debug[Int].show(1)
  // Debug.apply[Int].show(1)
  // Debug.apply[Int](IntDebug).show(1)





  sealed trait PathLike[A] {
    def concat(first: A, second: A): A

    def root: A

  }
  // concat(concat(a,b),c) == concact(a, concat(b,c))
  // concat(a, root) == a
  // concat(root, a) == a



  object PathLike {

    def apply[A: PathLike]: PathLike[A] = implicitly[PathLike[A]]


  }
}

object exercise2 {
  import exercise2._
}

object exercise3 {
  import exercise1._
  import exercise2._

  sealed trait Node
  final case object Root extends Node
  final case class Child(parent: Node, name: String) extends Node

  implicit val NodePathLike: PathLike[Node] = new PathLike[Node] {
    override def concat(first: Node, second: Node): Node =
      (first, second) match {
        case (Root, y) => y
        case (x, Root) => x
        case (x, Child(p2, n2)) => Child(concat(x, p2), n2)

      }

    override def root: Node = ???
  }
}

object exercise4 {
  import exercise1._
  import exercise2._
  import exercise3._

  implicit class PathLikeSyntax[A: PathLike](self: A) {
    def / (next: A): A = PathLike[A].concat(self, next)


  }

  def root[A: PathLike]:A = PathLike[A].root



}
