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
    def toInt(value : String): ValidationNel[Error, Int] =
        Validation.fromTryCatchNonFatal(value.toInt).leftMap(_ => NonEmptyList[Error](NotANumber(value)))



    println("Please enter a row:")
    val row = readLine()
    println("Please enter a column:")
    val col = readLine()



    (toInt(col) |@| toInt(row))((_, _))


//    for {
//      rowInt <- toInt(row)
//      colInt <- toInt(col)
//    }yield (rowInt, colInt)

//    ???
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

  final case class IO[A](unsafePerformIO: () => A)


  // we create pure functional code
  def readLine2: IO[String] = IO( () => readLine())

  def printLine(line : String): IO[Unit] = IO(() => println(line))


  implicit val MonadIO: Monad[IO] = new Monad[IO] {
    override def bind[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] =
      IO[B] (() =>
        f(fa.unsafePerformIO()).unsafePerformIO()
      )


    override def point[A](a: => A): IO[A] = IO(() => a)
  }


  //IO monad example, how to write pure functional code
  // scalaZ task, async IO monad
  val program: IO[Unit] =
    for {
      _ <- printLine("What is your name")
      name <- readLine2
      _ <- printLine("hello : " + name)
    } yield ()




  final case class Thunk[A](run: () => A)

  implicit val MonadThunk: Monad[Thunk] = ???
}

object exercise4 {
  def filter[A](f: A => Boolean, l: List[A]): List[A] = {
    val foldable = Foldable[List]

    //def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B])

    // in scalaZ Sum
    //case class Sum(value: Int)

    //Foldable[List[Int]].foldMap(List(1,2,3))(Sum(_)).value

    foldable.foldMap[A, List[A]](l)(a => if(f(a)) List(a) else Nil )

    //???
  }
}

object exercise5 {



  trait DeferMap [F[_], B] {
    type A
    val fusedMap : A => B
    val fa : F[A]

    def run(implicit F: Functor[F]): F[B] =
      F.map(fa)(fusedMap)
  }

  object DeferMap {
    def defer[F[_], A0](fa0: F[A0]): DeferMap[F, A0]  = new DeferMap[F, A0] {

      override type A = A0

      override val fusedMap: A0 => A = x => x

      override val fa = fa0

    }

    implicit def DefedMapFunctor[F[_]]: Functor[DeferMap[F, ?]] = new Functor[DeferMap[F, ?]] {


      //      def map[A0, B0](fa: DeferMap[F, A0])(f: A0 => B0):
      //      type A = fa.A
      override def map[A0, B0](fa: DeferMap[F, A0])(f: A0 => B0): DeferMap[F, B0] = ???
    }


  }




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
