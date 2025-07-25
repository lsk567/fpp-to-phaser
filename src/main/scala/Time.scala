package fpp.compiler.analysis

sealed trait Time
case class MS(value: BigInt) extends Time
case class US(value: BigInt) extends Time
case class NS(value: BigInt) extends Time