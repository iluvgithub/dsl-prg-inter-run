package kyseliov.free

import cats.{Functor, Monad, Monoid}

import scala.annotation.tailrec

// https://blog.higher-order.com/assets/trampolines.pdf
sealed trait Free[F[_], A]:

  def flatMap[B](g: A => Free[F, B]): Free[F, B] = FlatMap(this, g)

  def map[B](f: A => B): Free[F, B] = flatMap(f andThen Free.pure)

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

  given [F[_]]: Monad[[X] =>> Free[F, X]] with

    def pure[A](a: A): Free[F, A] =
      Free.pure(a)

    def flatMap[A, B](fa: Free[F, A])(g: A => Free[F, B]): Free[F, B] =
      FlatMap(fa, g)

    def tailRecM[A, B](a: A)(f: A => Free[F, Either[A, B]]): Free[F, B] =
      FlatMap(f(a), {
        case Left(next) => tailRecM(next)(f)
        case Right(b) => Free.pure(b)
      })


  def freeMonad[F[_]] = summon[Monad[[X] =>> Free[F, X]]]


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


object StateSyntax:

  enum St[S, A]:
    case Get(f: S => A)
    case Put(s: S, a: A)

  type FreeState[S, A] = Free[[X] =>> St[S, X], A]

  def pureState[S, A](a: A): FreeState[S, A] = Free.pure(a)

  def putState[S](s: S): FreeState[S, Unit] = Free.impure(St.Put(s, pureState(())))

  def getState[S]: FreeState[S, S] = Free.impure(St.Get(pureState))


  given [S]: Functor[[X] =>> St[S, X]] with

    def map[A, B](fa: St[S, A])(f: A => B): St[S, B] =
      fa match
        case St.Get(g) =>
          St.Get(g.andThen(f))

        case St.Put(s, a) =>
          St.Put(s, f(a))

  @tailrec
  def runState[S, A](s: S, fr: FreeState[S, A]): (A, S) =
    Free.out(fr) match {
      case Left(St.Get(f)) => runState(s, f(s))
      case Left(St.Put(s0, fa)) => runState(s0, fa)
      case Right(a) => (a, s)
    }

  def zipIndex[A](as: List[A]): List[(Int, A)] =
    val z0: FreeState[Int, List[(Int, A)]] = pureState[Int, List[(Int, A)]](List())
    val st: FreeState[Int, List[(Int, A)]] = as.foldLeft(z0)((acc, a) =>
      for {
        xs <- acc
        n <- getState
        _ <- putState(n + 1)
      } yield (n, a) :: xs
    )
    runState(0, st)._1.reverse


object ReaderWriterSyntax:

  enum RW[I, O, X]:
    case Get(f: I => X)
    case Put(o: O, x: X)

  given [I, O]: Functor[[X] =>> RW[I, O, X]] with

    def map[A, B](fa: RW[I, O, A])(f: A => B): RW[I, O, B] =
      fa match
        case RW.Get(g) =>
          RW.Get(g.andThen(f))

        case RW.Put(s, a) =>
          RW.Put(s, f(a))


  type IT[I, O, A] = Free[[X] =>> RW[I, O, X], A]

  def pureIT[I, O, A](a: A): IT[I, O, A] = Free.pure(a)

  def getIT[I, O]: IT[I, O, I] = Free.impure(RW.Get(pureIT))

  def putIT[I, O](o: O): IT[I, O, Unit] = Free.impure(RW.Put(o, pureIT(())))

  def runIt[I, O, A](i: I, it: IT[I, O, A])(using MON: Monoid[O]): (A, O) =
    @tailrec
    def loop(acc: O, it: IT[I, O, A]): (A, O) = Free.out(it) match {
      case Right(a) => (a, acc)
      case Left(RW.Get(f)) => loop(acc, f(i))
      case Left(RW.Put(o, a)) => loop(MON.combine(acc, o), a)
    }

    loop(MON.empty, it)
