package kyseliov.example


enum Eff[R, A]:
  case Pure(a: A)
  case Impure[R, X, A](union: Union[R, X], k: X => Eff[R, A]) extends Eff[R, A]

  def flatMap[B](f: A => Eff[R, B]): Eff[R, B] = this match
    case Pure(a)       => f(a)
    case Impure(u, k)  => Impure(u, x => k(x).flatMap(f))

  def map[B](f: A => B): Eff[R, B] = flatMap(a => Pure(f(a)))

object Eff:
  def pure[R, A](a: A): Eff[R, A] = Pure(a)

  // Lift any effect F[A] into Eff[R, A] as long as F is a Member of R
  def send[F[_], R, A](fa: F[A])(using m: Member[F, R]): Eff[R, A] =
    Impure(m.inj(fa), Pure(_))

  // Run a computation once all effects have been handled (R = FxNil)
  def run[A](eff: Eff[FxNil, A]): A = eff match
    case Pure(a)      => a
    case Impure(_, _) => sys.error("Impossible: FxNil contains no effects")
