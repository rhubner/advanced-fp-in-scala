package lambdaconf.data

import matryoshka._
import monocle._
import scalaz.{Lens => _, _}

import Scalaz._

object exercise1 {
  // 1. All prime numbers - Codata
  // 2. A JSON document - Data
}

object exercise2 {
  sealed trait Boolean {
    def apply[A](ifTrue: => A, ifFalse: => A): A
  }
  object Boolean {
    val True = new Boolean {
      override def apply[A](idFture: => A, ifFalse: => A): A = idFture
    }
    val False = new Boolean {
      // ???
      override def apply[A](idFture: => A, ifFalse: => A): A = ifFalse
    }
  }



  sealed trait Either[A, B] {
    def apply[Z](left: A => Z, right: B => Z): Z

  }
  object Either {
    def left[A, B](v: A): Either[A, B] = new Either[A, B] {
      override def apply[Z](left: (A) => Z, right: (B) => Z): Z = left(v)
    }
    def right[A, B](v: B): Either[A, B] = new Either[A, B] {
      override def apply[Z](left: (A) => Z, right: (B) => Z): Z = right(v)
    }

  }


  val result: Either[String, Int] = Either.left("bleee")




  // Cheat: type Option[A] = Either[Unit, A]
  sealed trait Option[A] {
    def apply[Z](some: A => Z, none: => Z ): Z
  }
  object Option {
    def none[A]: Option[A] = new Option[A] {
      override def apply[Z](some: (A) => Z, none: => Z): Z = none
    }
    def some[A](v: A): Option[A] = new Option[A] {
      override def apply[Z](some: (A) => Z, none: => Z): Z = some(v)
    }
  }
}

object exercise3 {
  trait Natural { self =>
    def fold[Z](zero: => Z, succ: Z => Z): Z

    def succ: Natural = new Natural {
      def fold[Z](zero: => Z, succ: Z => Z): Z = succ(self.fold(zero, succ))
    }
    def + (that: Natural): Natural = new Natural {
      def fold[Z](zero: => Z, succ: Z => Z): Z = that.fold[Natural](self, _.succ).fold[Z](zero, succ)
    }
    def * (that: Natural): Natural = new Natural {
      def fold[Z](zero: => Z, succ: Z => Z): Z =
        that.fold[Natural](Natural.zero, _ + self).fold[Z](zero, succ)
    }
    def isZero: Boolean = fold[Boolean](true, _ => false)
    def toInt: Int = fold[Int](0, _ + 1)
    override def toString = toInt.toString
  }
  object Natural {
    val zero = new Natural { def fold[Z](zero: => Z, succ: Z => Z): Z = zero }
    def of(v: Int): Natural = if (v == 0) zero else of(v - 1).succ
  }

  trait Integer { self =>




    // ???
  }

  //A - B
  case class Integer0(pos: Natural, neg: Natural) {
    def + (that: Integer0) = Integer0(pos + that.neg, neg + that.pos)

    def unary_- = Integer0(neg, pos)

  }

  case class Rational(num: Integer0, denom: Integer0)

  //case class Real(lessThat: Rational => Boolean, greaterThan: Rational => Boolean)



  case class Identity[A](value: A)

  implicit val IdentityMonad = new Monad[Identity]  {
    override def bind[A, B](fa: Identity[A])(f: (A) => Identity[B]): Identity[B] = f(fa.value)

    override def point[A](a: => A): Identity[A] = Identity(a)
  }


  //F ~> B
  trait NaturalTransformation[F[_], G[_]] {
    //def apply[]()
  }


  //F ~> Identity
  // Option ~> Identity ----- NOOOOOO.

}

object exercise4 {
  sealed trait JSON
  final case object Null extends JSON
  final case class Array(value: List[JSON]) extends JSON
  final case class Object(value: List[(String, JSON)]) extends JSON
  final case class Number(value: String) extends JSON
  final case class Boolean(value: Boolean) extends JSON

  val _Null     : Prism[JSON, Unit] = ???
  val _Array    : Prism[JSON, List[JSON]] = ???
  val _Object   : Prism[JSON, List[(String, JSON)]] = ???
  val _Number   : Prism[JSON, String] = ???
  val _Boolean  : Prism[JSON, Boolean] = ???
}

object exercise5 {
  import exercise4._

  sealed trait ContactType
  case object Business extends ContactType
  case object Personal extends ContactType

  case class Person(name: String, age: Int, contactType: ContactType)

  val _name : Lens[Person, String] = ???
  val _age : Lens[Person, Int] = ???
  val _contactType : Lens[Person, ContactType] = ???

  val _Business : Prism[ContactType, Unit] = ???
  val _Personal : Prism[ContactType, Unit] = ???

  def encodePerson(v: Person): JSON = ???

  def decodePerson(v: JSON): Option[Person] = ???
}

object exercise6 {

  sealed trait JSON[A]
  final case class Null[A]() extends JSON[A]
  final case class Array[A](value: List[A]) extends JSON[A]
  final case class Object[A](value: List[(String, A)]) extends JSON[A]
  final case class Number[A](value: String) extends JSON[A]
  final case class Boolean[A](value: scala.Boolean) extends JSON[A]

  case class Fix[F[_]](value: F[Fix[F]])

  type JSONR = Fix[JSON]

  // type JsonXml = Fix[Coproduct[JSON, XML]]


  // type JSON = Coproduct of all case classes to represent JSON




  //def array[A](v: List[A]): Fix[JSON] = Fix[JSON]

  implicit val FunctorJSON = new Functor[JSON] {
    override def map[A, B](fa: JSON[A])(f: (A) => B): JSON[B] = ???
  }

  val exampl: JSONR = Fix[JSON](Array(List(Fix[JSON](Boolean(true)))))

  //Cofree[JSON, ?]



  val TraverseJson: Traverse[JSON] = ???



}

object exercise7 {
  import exercise6._

  type RecursiveJSON = Fix[JSON]

  val ExampleJSON : RecursiveJSON = ???

//  def detectStringNumbers(v: RecursiveJSON): RecursiveJSON = {
//    val functorT: FunctorT[Fix] = implicitly[FunctorT[Fix]]
//
//    import functorT._
//
//
//    ???
//  }
}
