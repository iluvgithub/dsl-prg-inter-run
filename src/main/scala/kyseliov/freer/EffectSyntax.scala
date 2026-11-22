package kyseliov.freer

object EffectSyntax:
  // ---------------------------------------------------------------------------
  // 5. Some effects
  // ---------------------------------------------------------------------------

  // ---- Reader ----
  enum Reader[E, X]:
    case Ask() extends Reader[E, E]

  type ReaderOf[E] = [X] =>> Reader[E, X]

  def ask[E, R <: Effects](using Member[ReaderOf[E], R]): Eff[R, E] =
    send[ReaderOf[E], R, E](Reader.Ask())

  def runReader[E, R <: Effects, A](e: E)(m: Eff[ReaderOf[E] :+: R, A]): Eff[R, A] =
    handleRelay[ReaderOf[E], R, A, A](m)(a => Pure(a))(
      [X] => (fx: Reader[E, X], k: X => Eff[R, A]) =>
        fx match
          case Reader.Ask() => k(e))

  // ---- State ----
  enum State[S, X]:
    case Get() extends State[S, S]
    case Put(s: S) extends State[S, Unit]

  type StateOf[S] = [X] =>> State[S, X]

  def get[S, R <: Effects](using Member[StateOf[S], R]): Eff[R, S] =
    send[StateOf[S], R, S](State.Get())

  def put[S, R <: Effects](s: S)(using Member[StateOf[S], R]): Eff[R, Unit] =
    send[StateOf[S], R, Unit](State.Put(s))

  def modify[S, R <: Effects](f: S => S)(using Member[StateOf[S], R]): Eff[R, Unit] =
    get[S, R].flatMap(s => put[S, R](f(s)))

  def runState[S, R <: Effects, A](s0: S)(m: Eff[StateOf[S] :+: R, A]): Eff[R, (A, S)] =
    handleRelayS[StateOf[S], R, S, A, (A, S)](s0, m)((s, a) => Pure((a, s)))(
      [X] => (s: S, fx: State[S, X], k: (S, X) => Eff[R, (A, S)]) =>
        fx match
          case State.Get() => k(s, s)
          case State.Put(s2) => k(s2, ()))

  // ---- Writer ----
  enum Writer[W, X]:
    case Tell(w: W) extends Writer[W, Unit]

  type WriterOf[W] = [X] =>> Writer[W, X]

  def tell[W, R <: Effects](w: W)(using Member[WriterOf[W], R]): Eff[R, Unit] =
    send[WriterOf[W], R, Unit](Writer.Tell(w))

  def runWriter[W, R <: Effects, A](m: Eff[WriterOf[W] :+: R, A]): Eff[R, (A, List[W])] =
    handleRelayS[WriterOf[W], R, List[W], A, (A, List[W])](Nil, m)((ws, a) => Pure((a, ws.reverse)))(
      [X] => (ws: List[W], fx: Writer[W, X], k: (List[W], X) => Eff[R, (A, List[W])]) =>
        fx match
          case Writer.Tell(w) => k(w :: ws, ()))

  // ---- Exception ----
  enum Exc[E, X]:
    case Throw(e: E) extends Exc[E, Nothing]

  type ExcOf[E] = [X] =>> Exc[E, X]

  def throwError[E, R <: Effects, A](e: E)(using Member[ExcOf[E], R]): Eff[R, A] =
    send[ExcOf[E], R, Nothing](Exc.Throw(e)).map[A](n => n)

  def runExc[E, R <: Effects, A](m: Eff[ExcOf[E] :+: R, A]): Eff[R, Either[E, A]] =
    handleRelay[ExcOf[E], R, A, Either[E, A]](m)(a => Pure(Right(a)))(
      [X] => (fx: Exc[E, X], k: X => Eff[R, Either[E, A]]) =>
        fx match
          case Exc.Throw(e) => Pure(Left(e)))

