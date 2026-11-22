package kyseliov.freer

import kyseliov.freer.EffectSyntax._

import scala.annotation.tailrec
//https://okmij.org/ftp/Haskell/extensible/more.pdf

sealed trait Arrs[F[_], A, B]

final case class Leaf[F[_], A, B](f: A => Freer[F, B]) extends Arrs[F, A, B]

final case class Node[F[_], A, X, B](l: Arrs[F, A, X], r: Arrs[F, X, B]) extends Arrs[F, A, B]

extension [F[_], A, B](q: Arrs[F, A, B])
  /** snoc: append one arrow on the right (the paper's |>) */
  def |>[C](f: B => Freer[F, C]): Arrs[F, A, C] = Node(q, Leaf(f))
  /** concatenation (the paper's ><) */
  def ><[C](r: Arrs[F, B, C]): Arrs[F, A, C] = Node(q, r)

/** Result of taking the leftmost arrow off a queue (the paper's TViewL). */
sealed trait ViewL[F[_], A, B]

final case class One[F[_], A, B](f: A => Freer[F, B]) extends ViewL[F, A, B]

final case class Cons[F[_], A, X, B](f: A => Freer[F, X], rest: Arrs[F, X, B]) extends ViewL[F, A, B]

/** Re-associates to the right while looking for the head: amortized O(1). */
@tailrec
def viewl[F[_], A, B](q: Arrs[F, A, B]): ViewL[F, A, B] = q match
  case Leaf(f) => One(f)
  case Node(Leaf(f), r) => Cons(f, r)
  case Node(Node(a, b), c) => viewl(Node(a, Node(b, c)))

/** Apply a whole queue of continuations to a value (the paper's qApp). */
@tailrec
def qApp[F[_], A, B](q: Arrs[F, A, B], x: A): Freer[F, B] =
  viewl(q) match
    case One(h) => h(x)
    case Cons(h, t) =>
      h(x) match
        case Pure(y) => qApp(t, y)
        case Impure(fx, q2) => Impure(fx, q2 >< t)

// ---------------------------------------------------------------------------
// 2. The Freer monad itself
// ---------------------------------------------------------------------------

sealed trait Freer[F[_], A]:
  def flatMap[B](f: A => Freer[F, B]): Freer[F, B] = this match
    case Pure(a) => f(a)
    case Impure(fx, k) => Impure(fx, k |> f)

  def map[B](f: A => B): Freer[F, B] = flatMap(a => Pure(f(a)))

final case class Pure[F[_], A](a: A) extends Freer[F, A]

final case class Impure[F[_], X, A](fx: F[X], k: Arrs[F, X, A]) extends Freer[F, A]

object Freer:
  /** Lift a single operation. */
  def lift[F[_], A](fa: F[A]): Freer[F, A] = Impure(fa, Leaf((a: A) => Pure(a)))

  /** Generic interpreter into any monad M (natural transformation F ~> M). */
  def foldMap[F[_], M[_], A](m: Freer[F, A])(nt: [X] => F[X] => M[X])(
    using pure: [X] => X => M[X], bind: [X, Y] => (M[X], X => M[Y]) => M[Y]): M[A] =
    m match
      case Pure(a) => pure(a)
      case Impure(fx, k) => bind(nt(fx), x => foldMap(qApp(k, x))(nt))

// ---------------------------------------------------------------------------
// 3. Open union of effects (index-tagged, as in the paper's section on open unions)
// ---------------------------------------------------------------------------

/** A type-level list of effect constructors. */
sealed trait Effects

sealed trait NoFx extends Effects

sealed trait :+:[F[_], R <: Effects] extends Effects

/** An effect value `F[X]` for some F in R, stored with the index of F in R. */
final class Union[R <: Effects, X](val tag: Int, val value: Any)

/** Witness that F occurs in R, carrying its position. */
trait Member[F[_], R <: Effects]:
  def index: Int

trait LowPriorityMember:
  given tail[F[_], G[_], R <: Effects](using m: Member[F, R]): Member[F, G :+: R] with
    def index: Int = m.index + 1

object Member extends LowPriorityMember:
  given head[F[_], R <: Effects]: Member[F, F :+: R] with
    def index: Int = 0

object Union:
  def inj[F[_], R <: Effects, X](fx: F[X])(using m: Member[F, R]): Union[R, X] =
    Union(m.index, fx)

  def prj[F[_], R <: Effects, X](u: Union[R, X])(using m: Member[F, R]): Option[F[X]] =
    if u.tag == m.index then Some(u.value.asInstanceOf[F[X]]) else None

  /** Peel off the first effect: either it is F, or it is one of the rest. */
  def decomp[F[_], R <: Effects, X](u: Union[F :+: R, X]): Either[Union[R, X], F[X]] =
    if u.tag == 0 then Right(u.value.asInstanceOf[F[X]])
    else Left(Union[R, X](u.tag - 1, u.value))

/** Eff r a = Freer (Union r) a */
type UnionK[R <: Effects] = [X] =>> Union[R, X]
type Eff[R <: Effects, A] = Freer[UnionK[R], A]

/** Run a computation with no remaining effects. */
def run[A](m: Eff[NoFx, A]): A = m match
  case Pure(a) => a
  case Impure(_, _) => throw new IllegalStateException("impossible: no effects left")

/** Issue an effectful request. */
def send[F[_], R <: Effects, A](fa: F[A])(using Member[F, R]): Eff[R, A] =
  Freer.lift[UnionK[R], A](Union.inj[F, R, A](fa))

// ---------------------------------------------------------------------------
// 4. Generic handlers (the paper's handleRelay / handleRelayS)
// ---------------------------------------------------------------------------

/** Handle the first effect F; forward the others untouched. */
def handleRelay[F[_], R <: Effects, A, B](m: Eff[F :+: R, A])(ret: A => Eff[R, B])(
  h: [X] => (F[X], X => Eff[R, B]) => Eff[R, B]): Eff[R, B] =
  def loop(m: Eff[F :+: R, A]): Eff[R, B] = m match
    case Pure(a) => ret(a)
    case Impure(u, q) =>
      Union.decomp(u) match
        case Right(fx) => h(fx, x => loop(qApp(q, x)))
        case Left(u2) => Impure(u2, Leaf((x: Any) => loop(qApp(q, x.asInstanceOf))))

  loop(m)

/** Same, threading a state through the handler. */
def handleRelayS[F[_], R <: Effects, S, A, B](s0: S, m: Eff[F :+: R, A])(ret: (S, A) => Eff[R, B])(
  h: [X] => (S, F[X], (S, X) => Eff[R, B]) => Eff[R, B]): Eff[R, B] =
  def loop(s: S, m: Eff[F :+: R, A]): Eff[R, B] = m match
    case Pure(a) => ret(s, a)
    case Impure(u, q) =>
      Union.decomp(u) match
        case Right(fx) => h(s, fx, (s2, x) => loop(s2, qApp(q, x)))
        case Left(u2) => Impure(u2, Leaf((x: Any) => loop(s, qApp(q, x.asInstanceOf))))

  loop(s0, m)

// ---------------------------------------------------------------------------
// 6. Demo
// ---------------------------------------------------------------------------


@main def demoFreer(): Unit =
  // Handlers are applied in the order of the effect list.
  import SamplePrograms._
  val r1: Either[String, ((Int, Int), List[String])] = run(runExc(runWriter(runState(5)(runReader(10)(program[Fx])))))
  println(r1) // Right(((30,15),List(env=10 state=5, new state=15)))   (shape: Either[String, ((Int,Int), List[String])])

  // Different handler order = different semantics: here the state survives the exception.
  type Fx2 = StateOf[Int] :+: ExcOf[String] :+: NoFx
  val r2: Either[String, (Int, Int)] = run(runExc(runState(0)(failing[Fx2])))
  println(r2) // Left(boom)           (state lost)

  type Fx3 = ExcOf[String] :+: StateOf[Int] :+: NoFx
  val r3: (Either[String, Int], Int) = run(runState(0)(runExc(failing[Fx3])))
  println(r3) // (Left(boom),1)       (state kept)

  // Left-nested binds are cheap thanks to the type-aligned queue.
  val n = 20 //0000
  val deep: Eff[ReaderOf[Int] :+: NoFx, Int] =
    (1 to n).foldLeft(ask[Int, ReaderOf[Int] :+: NoFx])((m, _) => m.flatMap(x => Pure(x + 1)))
  val r4: Int = run(runReader(0)(deep))
  println(r4) // 200000

