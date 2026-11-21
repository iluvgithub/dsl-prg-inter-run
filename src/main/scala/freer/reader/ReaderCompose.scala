package freer.reader

import cats.Monad

object ReaderCompose :

  def readerComposeMonad[M[_] : Monad, R]: Monad[[A] =>> R => M[A]] = new Monad[[A] =>> R => M[A]] {
    private type ReaderM[X] = R => M[X]

    def pure[A](a: A): ReaderM[A] = _ => Monad[M].pure(a)

    def flatMap[A, B](fa: ReaderM[A])(f: A => ReaderM[B]):
    ReaderM[B] = r => Monad[M].flatMap(fa(r)) { a => f(a)(r) }

    def tailRecM[A, B](a: A)(f: A => ReaderM[Either[A, B]]): ReaderM[B] =
      r => Monad[M].tailRecM(a)(a => f(a)(r))
  }