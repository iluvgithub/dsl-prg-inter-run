package kyseliov.freer

import cats.Functor

import scala.annotation.tailrec

// https://blog.higher-order.com/assets/trampolines.pdf
sealed trait Free[F[_], A]:

  def flatMap[B](g: A => Free[F, B]): Free[F, B] = FlatMap(this, g)

case class Pure[F[_], A](a: A) extends Free[F, A]

case class Impure[F[_], A](k: F[Free[F, A]]) extends Free[F, A]

case class FlatMap[F[_], X, A](fx: Free[F, X], g: X => Free[F, A]) extends Free[F, A]

object Free:

  def pure[F[_], A](a: A): Free[F, A] = Pure(a)

  def impure[F[_], A](k: F[Free[F, A]]): Free[F, A] = Impure(k)

  def kleisliCompose[F[_], A, B, C](f: A => Free[F, B], g: B => Free[F, C]): A => Free[F, C] =
    a => FlatMap(f(a), g)

  @tailrec
  def out[F[_], A](fa: Free[F, A])(using F: Functor[F]): Either[F[Free[F, A]], A] = fa match {
    case Pure(a) => Right(a)
    case Impure(k) => Left(k)
    case FlatMap(fx, g) => fx match {
      case Pure(x) => out(g(x))
      case Impure(fkx) => Left(F.map(fkx)(kx => FlatMap(kx, g)))
      case FlatMap(fy, h) => out(FlatMap(fy, kleisliCompose(h, g)))
    }
  }


object Trampoline:

  type Bounce[A] = Free[Function0, A]

  given Functor[Function0] with
    def map[A, B](fa: () => A)(f: A => B): () => B =
      () => f(fa())

  def done[A](a: A): Bounce[A] = Free.pure(a)

  def more[A](k: () => Bounce[A]): Bounce[A] = Free.impure(k)

  @tailrec
  def run[A](ba: Bounce[A]): A = Free.out(ba) match {
    case Right(a) => a
    case Left(ffr) => run(ffr())
  }
