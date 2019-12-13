import cats._
import cats.syntax.all._

sealed trait BIO[E, A] {
  def unsafeRun(): Either[E, A] = this match {
    case BIO.Pure(value) => Right(value)
    case BIO.FlatMap(fa, f) =>
      fa.unsafeRun().flatMap { a =>
        f(a).unsafeRun()
      }
    case BIO.Effect(run) => Right(run())
    case BIO.Failure(cause) => Left(cause)
    case BIO.Fold(fa, fh, sh) =>
      fa.unsafeRun().fold(fh, sh).unsafeRun()
  }
}

object BIO {

  private case class Pure[E, A](value: A) extends BIO[E, A]
  private case class FlatMap[E, A, B](fa: BIO[E, A], f: A => BIO[E, B]) extends BIO[E, B]
  private case class Effect[E, A](run: () => A) extends BIO[E, A]
  private case class Failure[E, A](cause: E) extends BIO[E, A]
  private case class Fold[E, A, E1, A1](fa: BIO[E, A],
    failure: E => BIO[E1, A1],
    success: A => BIO[E1, A1]
  ) extends BIO[E1, A1]

  def effect[E, A](run: () => A): BIO[E, A] = BIO.Effect(run)

  implicit def bioMonadError[E]: MonadError[({type F[A] = BIO[E, A]})#F, E] = {
    new MonadError[({type F[A] = BIO[E, A]})#F, E] with StackSafeMonad[({type F[A] = BIO[E, A]})#F] {
      def pure[A](x: A): BIO[E, A] = BIO.Pure(x)
      def flatMap[A, B](fa: BIO[E, A])(f: A => BIO[E, B]): BIO[E, B] = BIO.FlatMap(fa, f)
      def raiseError[A](e: E) = BIO.Failure(e)
      def handleErrorWith[A](fa: BIO[E, A])(f: E => BIO[E, A]): BIO[E, A] = {
        BIO.Fold[E, A, E, A](fa, f, _ => fa)
      }
    }
  }

  implicit def bioBiFunctor: Bifunctor[BIO] = new Bifunctor[BIO] {

    def bimap[A, B, C, D](fab: BIO[A, B])(f: A => C, g: B => D): BIO[C, D] = {
      val mea = bioMonadError[A]
      BIO.Fold(fab, { a: A =>
        BIO.Failure(f(a))
      }, { b: B =>
        BIO.Pure(g(b))
      })
    }
  }
}

object BConsole {
  def putStrLn[E](str: String): BIO[E, Unit] = BIO.effect(() => println(str))
  def getStr[E](): BIO[E, String] = BIO.effect(() => scala.io.StdIn.readLine())
}

import cats.syntax.all._

object BIOSmapleApp {
  def sum(readInt: BIO[String, Int]) = {
    for {
      ii <- readInt
      jj <- readInt
    } yield ii + jj
  }

  def readInt: BIO[String, Int] = {
    BConsole.getStr[String]().flatMap { in =>
      val eab = Either.catchNonFatal(in.toInt).leftMap(_ => s"${in} is not a number")
      MonadError[({type F[A] = BIO[String, A]})#F, String].fromEither(eab)
    }
  }

  val app = sum(readInt).flatMap { r =>
    BConsole.putStrLn(s"Sum of input: ${r}")
  }.handleErrorWith { e =>
    BConsole.putStrLn(s"Error: $e")
  }

  def main(args: Array[String]): Unit = {
    app.unsafeRun()
  }

}
