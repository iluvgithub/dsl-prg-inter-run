package kyseliov.example

import kyseliov.*

// Effect 1: Reader / Environment
case class Ask[E, A](extract: E => A)
def ask[E, R](using Member[[X] =>> Ask[E, X], R]): Eff[R, E] =
  Eff.send(Ask[E,E](identity))

// Effect 2: Console Logging
case class Log[A](msg: String, next: A)
def log[R](msg: String)(using Member[Log, R]): Eff[R, Unit] =
  Eff.send(Log(msg, ()))

// Interpreter for Ask: handles the head effect and leaves the rest R intact
def runAsk[E, R, A](env: E)(eff: Eff[FxCons[[X] =>> Ask[E, X], R], A]): Eff[R, A] =
  eff match
    case Eff.Pure(a) => Eff.Pure(a)
    case Eff.Impure(u, k) =>
      Union.decomp(u) match
        case Right(Ask(extract)) => runAsk(env)(k(extract(env)))
        case Left(otherUnion)    => Eff.Impure(otherUnion, x => runAsk(env)(k(x)))

// Interpreter for Log: collects logs into a List[String]
def runLog[R, A](eff: Eff[FxCons[Log, R], A]): Eff[R, (List[String], A)] =
  eff match
    case Eff.Pure(a) => Eff.Pure((Nil, a))
    case Eff.Impure(u, k) =>
      Union.decomp(u) match
        case Right(Log(msg, next)) =>
          runLog(k(next)).map((logs, res) => (msg :: logs, res))
        case Left(otherUnion) =>
          Eff.Impure(otherUnion, x => runLog(k(x)))
