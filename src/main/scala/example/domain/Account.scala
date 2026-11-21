package example.domain

import cats.free.Free

// =============================================================================
// 1. ALGEBRA
//    Only data types and operation signatures. No logic, no IO, no state.
//    Operations are reified as a GADT-style enum (AccountOp); `Free` turns
//    them into something that can be sequenced with for-comprehensions.
// =============================================================================

final case class Account(no: String, balance: BigDecimal)

enum AccountOp[A]:
  case Credit(no: String, amt: BigDecimal) extends AccountOp[Account]
  case Debit(no: String, amt: BigDecimal) extends AccountOp[Account]

type AccountF[A] = Free[AccountOp, A]

// Smart constructors: the "vocabulary" that programs are written in.
def credit(no: String, amt: BigDecimal): AccountF[Account] =
  Free.liftF[AccountOp, Account](AccountOp.Credit(no, amt))

def debit(no: String, amt: BigDecimal): AccountF[Account] =
  Free.liftF[AccountOp, Account](AccountOp.Debit(no, amt))