import munit.FunSuite

import cats.Eq
import cats.syntax.all._
import cats.laws.discipline.MonadErrorTests
import munit.DisciplineSuite
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

class IOSuite extends DisciplineSuite {

  implicit def arbitraryForIO[A: Arbitrary: Cogen]: Arbitrary[IO[A]] =
    Arbitrary(Gen.delay(genIO[A]))

  implicit val eqThrowable: Eq[Throwable] = new Eq[Throwable] {
    def eqv(l: Throwable, r: Throwable) = l.toString() == r.toString()
  }

  implicit def ioEq[A](implicit eqa: Eq[A]) =
    new Eq[IO[A]] {
      def eqv(l: IO[A], r: IO[A]): Boolean = {
        Either.catchNonFatal(IO.Runtime.unsafeRun(l)) == Either.catchNonFatal(
          IO.Runtime.unsafeRun(r)
        )
      }
    }

  def genIO[A: Arbitrary: Cogen]: Gen[IO[A]] = {
    Gen.frequency(
      1 -> genPure[A],
      1 -> genFailure[A],
      1 -> genRecover[A],
      2 -> genFlatMap[A],
      1 -> genEffect[A]
    )
  }

  def genFailure[A]: Gen[IO[A]] = {
    arbitrary[Throwable].map(e => IO.raiseError[A](e))
  }

  def genRecover[A: Arbitrary: Cogen]: Gen[IO[A]] = {
    for {
      ioa <- arbitrary[IO[A]]
      f <- arbitrary[Throwable => IO[A]]
    } yield ioa.handleErrorWith(f)
  }

  def genPure[A: Arbitrary: Cogen]: Gen[IO[A]] =
    arbitrary[A].map(_.pure[IO])

  def genFlatMap[A: Arbitrary: Cogen]: Gen[IO[A]] = {
    for {
      ioa <- arbitrary[IO[A]]
      f <- arbitrary[A => IO[A]]
    } yield ioa.flatMap(f)
  }

  def genEffect[A: Arbitrary: Cogen]: Gen[IO[A]] =
    arbitrary[Either[Throwable, A]].map { eoa =>
      IO.effect { () =>
        eoa match {
          case Left(e)  => throw e
          case Right(v) => v
        }
      }
    }

  checkAll(
    "IO.monadErrorLaws",
    MonadErrorTests[IO, Throwable].monadError[Int, Int, String]
  )
}
