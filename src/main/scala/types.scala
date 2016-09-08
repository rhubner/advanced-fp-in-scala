package lambdaconf.types

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

object exercise1 {
  final case class CheckersBoard(/* ??? */)

  sealed trait CheckerPiece
  // ???
}

object exercise2 {
  //Box is type constructor, one parameter, start

  final case class Box[A](a: A)
}

object exercise3 {
  // 1. scala.collection.List  // * => *
  // 2. F[_, _] [*, *] => *
  // 3. Option * => *
  // 4. Int *
  // 5. T[_[_], _] [(* => *), *] => *
}

object exercise4 {
  trait FileSystem {

    type File

    def ls: List[File]

    // ???
  }

  def fs: FileSystem = ???

  // val myFiles: List[fs#File] = ???


}

object exercise5 {
  sealed trait Example[F[_]] {
    def value: F[String]
  }

//new Example[Either[?, Int]] {

  type L[A] = Either[A, Int]

// val moje = new Example[({type L[A]  = Either[A, Int]})#L ] { // <-- ???
  val moje = new Example[L] { // <-- ???
    def value: Either[String, Int] = Right(2)
  }

}
