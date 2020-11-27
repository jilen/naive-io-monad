import cats._
import cats.syntax.all._



sealed trait IO[A]


object IO {

  object Runtime {
    def unsafeRun[A](io: IO[A]): A = io match {
      case IO.Pure(a) => a
      case IO.FlatMap(fa, f) => unsafeRun(f(unsafeRun(fa)))
      case IO.Effect(run) => run()
      case IO.Failure(ex) => throw ex
      case IO.Recover(fa, h) =>
        try {
          unsafeRun(fa)
        } catch {
          case e: Throwable =>
            unsafeRun(h(e))
        }
    }
  }

  private case class Pure[A](value: A) extends IO[A]
  private case class FlatMap[A, B](fa: IO[A], f: A => IO[B]) extends IO[B]
  private case class Effect[A](run: () => A) extends IO[A]
  private case class Failure[A](ex: Throwable) extends IO[A]
  private case class Recover[A](fa: IO[A], handler: Throwable => IO[A]) extends IO[A]

  def effect[A](f: () => A): IO[A] = Effect(f)
  def raiseError[A](e: Throwable): IO[A] = Failure(e)

  implicit def ioMonadErrorInstance: MonadError[IO, Throwable] = new MonadError[IO, Throwable] with StackSafeMonad[IO] {
    def pure[A](x: A) = IO.Pure(x)
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = IO.FlatMap(fa, f)
    def raiseError[A](e: Throwable) = IO.Failure(e)
    def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] = Recover(fa, f)
  }
}

object Console {
  def getStr(): IO[String] = IO.effect(() => scala.io.StdIn.readLine())
  def putStrLn(str: String): IO[Unit] = IO.effect(() => println(str))
}

import cats.syntax.all._

object IOAppDemo {
  def sum(i: IO[Int], j: IO[Int]): IO[Int] = {
    i.flatMap { ii =>
      j.map(jj => ii + jj)
    }
  }

  case class NotAInt(str: String) extends Throwable

  def main(args: Array[String]) = {
    val readInt: IO[Int] = Console.getStr().flatMap { str =>
      val r: Either[Throwable, Int] = try {
        Right(str.toInt)
      } catch {
        case e: Throwable => Left(NotAInt(str))
      }
      MonadError[IO, Throwable].fromEither(r) // 召唤 MonadError 实例
    }
    val app = sum(readInt, readInt).flatMap { r =>
      Console.putStrLn(r.toString)
    }.handleErrorWith {
      case NotAInt(str) => Console.putStrLn(s"$str 不是一个字符串")
    }
    IO.Runtime.unsafeRun(app)
  }

  def main2(args: Array[String]) = {
    val readInt = Console.getStr().map { str =>
      try{
        Right(str.toInt)
      } catch {
        case e: Exception => Left(s"$str is not a number")
      }
    }
  }
}
