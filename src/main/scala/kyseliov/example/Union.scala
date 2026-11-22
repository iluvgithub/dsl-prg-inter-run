package kyseliov.example

// r represents a type-level tuple/list of effect constructors (e.g., (Reader[Int, *], State[String, *]))
// Using an opaque type or erased tag + Any mimics Kiselyov's O(1) internal representation.
final class Union[R, A](val tag: Int, val value: Any)

// Type-level list markers
sealed trait FxNil

sealed trait FxCons[H[_], T] // H[_] : T

// Typeclass proving that effect F[_] is in the effect stack R at a known index
trait Member[F[_], R]:
  def index: Int

  final def inj[A](fa: F[A]): Union[R, A] = new Union(index, fa)

  final def prj[A](u: Union[R, A]): Option[F[A]] =
    if u.tag == index then Some(u.value.asInstanceOf[F[A]]) else None

object Member:
  // Base case: F is at the head of the stack (index 0)
  given head[F[_], T]: Member[F, FxCons[F, T]] with
    def index: Int = 0

  // Inductive case: F is further down the tail T (index + 1)
  given tail[F[_], H[_], T](using m: Member[F, T]): Member[F, FxCons[H, T]] with
    def index: Int = m.index + 1

object Union:
  // Decompose the head effect F[_] from FxCons[F, R]
  def decomp[F[_], R, A](u: Union[FxCons[F, R], A]): Either[Union[R, A], F[A]] =
    if u.tag == 0 then Right(u.value.asInstanceOf[F[A]])
    else Left(new Union(u.tag - 1, u.value))
