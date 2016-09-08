package lambdaconf.functions

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

object exercise1 {
  // Domain: {Vegetables, Fruits, Meat, Dairy, Eggs}
  // Codomain: {Love, Like, Neutral, Dislike, Hate}
}

object exercise2 {
  val compareStrings: (Char => Char) => (String, String) => Boolean = normalizer => (first, second) => {
    first.map(normalizer) == second.map(normalizer)
  }

  compareStrings(c => c)("ahoj", "Ahoj")


}

object exercise3 {

  //Function combinator, writing functions which take function and return function

  type Parser[A] = String => Either[String, (String, A)]

  def or[A](left: Parser[A], right: Parser[A]): Parser[A] = { input: String =>
    left(input) match {
      case Left(error) => right(input)
      case x => x
    }
  }

  def seq[A,B](first:Parser[A], second:Parser[B]): Parser[(A,B)] = ???

}

object exercise4 {
  object snd {
    def apply[A, B](v: (A, B)): B = v._2
  }


  trait Boolean {
    def apply[A](v: (A,A)): A
  }


  trait True extends Boolean {
    def apply[A](v: (A,A)): A = v._1
  }

  trait False extends Boolean {
    def apply[A](v: (A,A)): A = v._2
  }



}
