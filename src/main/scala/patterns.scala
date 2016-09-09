package lambdaconf.patterns

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

object exercise1 {

  // () => (Int, Int) //false promise :D :D
  // (2,3)

  case class NotANumber(value: String) extends Error

  def readRowCol(): ValidationNel[Error, (Int, Int)] = {

    //def toInt(value : String): Error \/ Int = \/.fromTryCatchNonFatal(value.toInt).leftMap(_ => NotANumber(value))
    def toInt(value : String): ValidationNel[Error, Int] = Validation.fromTryCatchNonFatal(value.toInt).leftMap(_ => NonEmptyList[Error](NotANumber(value)))



    println("Please enter a row:")
    val row = readLine()
    println("Please enter a column:")
    val col = readLine()



    val ccc = (toInt(col) |@| toInt(row))((_, ))


//    for {
//      rowInt <- toInt(row)
//      colInt <- toInt(col)
//    }yield (rowInt, colInt)

    ???
  }

  val aaa = Maybe.fromNullable(1000)
  val bbb: String \/ Int =  ??? ///  \/.right(100)

  //use some types for error

  sealed trait Error
  case object HostNotReachable extends Error
  case object UnspupportedHttpVersion extends Error
  case object ProtocolError extends Error

  val portNumber: Error \/ Int = ???

  // Validation[A,B]
  // Failure(value: A)
  // Success(value: B)

  val validation : Validation[String, Int] = ???


  def readPortNumber: Error \/ Int = ???

  def readHostname: Error \/ String = ???

  def readOtherConfig : Maybe[String] = ???

  val a = for {
    portNumber <- readPortNumber
    hostname <- readHostname
    otherConfig <- readOtherConfig.toRight(ProtocolError)
  } yield (portNumber, hostname, otherConfig)

  readPortNumber.flatMap(portNumber =>
    readHostname.map(hostname =>
      (portNumber, hostname)
    )
  )







}

object exercise2 {
  sealed trait Node
  final case object Root extends Node
  final case class Child(parent: Node, name: String) extends Node

  implicit val NodeMonoid: Monoid[Node] = ???
}

object exercise3 {
  final case class Thunk[A](run: () => A)

  implicit val MonadThunk: Monad[Thunk] = ???
}

object exercise4 {
  def filter[A](f: A => Boolean, l: List[A]): List[A] = {
    val foldable = Foldable[List]

    ???
  }
}

object exercise5 {
  trait List[A] { self =>
    def fold[Z](nil: => Z, cons: (Z, A) => Z): Z

    final def :: (next: A): List[A] = new List[A] {
      def fold[Z](nil: => Z, cons: (Z, A) => Z): Z = {
        cons(self.fold(nil, cons), next)
      }
    }
  }
  object List {
    def empty[A]: List[A] = new List[A] {
      def fold[Z](nil: => Z, cons: (Z, A) => Z): Z = nil
    }
  }

  implicit val ListTraverse: Traverse[List] = ???
}
