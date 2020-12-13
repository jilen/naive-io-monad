import cats._
import cats.syntax.all._
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable.Stack


sealed trait IO[A]


object IO {

  object RuntimeStackNotStafe {


    private def unsafeRunFlatMap[A, B](io: FlatMap[A, B], cb: Either[Throwable, B] => Unit): Unit = {
      unsafeRunAsync[A](io.fa, {
        case Left(e) =>
          cb(Left(e))
        case Right(a) =>
          unsafeRunAsync[B](io.f(a), {
            case Left(e) => cb(Left(e))
            case Right(b) => cb(Right(b))
          })
      })
    }

    def unsafeRunAsync[A](io: IO[A], cb: Either[Throwable, A] => Unit): Unit = io match {
      case IO.Pure(a) => cb(Right(a))
      case io: IO.FlatMap[_, _] => unsafeRunFlatMap(io, cb)
      case IO.Effect(run) => cb(Right(run()))
      case IO.Failure(ex) => cb(Left(ex))
      case IO.AsyncF(register) =>
        unsafeRunAsync[Unit](register(cb), _ => ())
      case IO.Recover(fa, h) =>
        unsafeRunAsync[A](fa, {
            case Left(e) => unsafeRunAsync(h(e), cb)
            case Right(v) => cb(Right(v))
        })
    }

  }

  object Runtime {

    private type Callback = Either[Throwable, Any] => Unit
    private type IOLoop = Stack[(() => IO[_], Callback)]

    private def runAsyncFlatMap[A, B](loop: IOLoop, io: FlatMap[A, B], fb: Either[Throwable, B] => Unit): Unit = {
      loop.push((() => io.fa) -> { eoa: Either[Throwable, Any] =>
        eoa match {
          case Left(e) =>
            fb(Left(e))
          case Right(a) =>
            loop.push((() => {
              io.f(a.asInstanceOf[A])
            }) -> { eoa: Either[Throwable, Any] =>
              loop.push((() => eoa.pure[IO]) ->{ eeob: Either[Throwable, Any] =>
                eeob match {
                  case Left(e) => throw new Exception("unreachable")
                  case Right(v) => fb(v.asInstanceOf[Either[Throwable, B]])
                }
              })
            })
        }
      })
    }

    def unsafeRunAsync[A](io: IO[A])(cb: Either[Throwable, A] => Unit) = {
      val loop = Stack[(() => IO[_], Callback)]()
      start(() => io, loop)(cb.asInstanceOf[Callback])
      var curr = null
      while(!loop.isEmpty) {
        val (io, cb) = loop.pop()
        start(io, loop)(cb)
      }
    }

    private def start[_]( io: () => IO[_], loop: IOLoop )(cb: Callback): Unit = io() match {
      case IO.Pure(a) => cb(Right(a))
      case io: IO.FlatMap[_, _] => runAsyncFlatMap(loop, io, cb)
      case IO.Effect(run) => cb(Right(run()))
      case IO.Failure(ex) => cb(Left(ex))
      case IO.AsyncF(register) =>
        loop.push(() => register(cb), _ => ())
      case IO.Recover(fa, h) =>
        loop.push((() => fa) -> { eoa: Either[Throwable, Any] =>
          eoa match {
            case Left(e) =>
              loop.push((() => h(e)) -> { ee: Either[Throwable, Any] =>
                cb(ee)
              })
            case Right(v) => cb(Right(v))
          }
        })
    }

    def unsafeRun[A](io: IO[A]): Either[Throwable, A] = {
      val latch = new CountDownLatch(1)
      val ref = new AtomicReference[Either[Throwable, A]]
      unsafeRunAsync(io) { r =>
        ref.set(r)
        latch.countDown()
      }
      latch.await()
      ref.get

    }
  }

  private case class Pure[A](value: A) extends IO[A]
  private case class FlatMap[A, B](fa: IO[A], f: A => IO[B]) extends IO[B]
  private case class Effect[A](run: () => A) extends IO[A]
  private case class Failure[A](ex: Throwable) extends IO[A]
  private case class Recover[A](fa: IO[A], handler: Throwable => IO[A]) extends IO[A]
  private case class AsyncF[A](f: (Either[Throwable, A] => Unit) => IO[Unit]) extends IO[A]
  //private case class Async[A](f: (Either[Throwable, A] => Unit) => Unit) extends IO[A]

  def effect[A](f: () => A): IO[A] = Effect(f)
  def raiseError[A](e: Throwable): IO[A] = Failure(e)
  def asyncF[A](cb: (Either[Throwable, A] => Unit) => IO[Unit]): IO[A] = IO.AsyncF(cb)

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

  def main(args: Array[String]): Unit = {
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
