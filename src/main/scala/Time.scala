package fpp.compiler.analysis

sealed trait Time {
  def toNanoseconds: BigInt

  def +(that: Time): Time = Time.fromNanoseconds(this.toNanoseconds + that.toNanoseconds)
  def -(that: Time): Time = Time.fromNanoseconds(this.toNanoseconds - that.toNanoseconds)
}

case class MS(value: BigInt) extends Time {
  def toNanoseconds: BigInt = value * 1_000_000
}

case class US(value: BigInt) extends Time {
  def toNanoseconds: BigInt = value * 1_000
}

case class NS(value: BigInt) extends Time {
  def toNanoseconds: BigInt = value
}

object Time {
  implicit val timeOrdering: Ordering[Time] = Ordering.by(_.toNanoseconds)

  def fromNanoseconds(ns: BigInt): Time = NS(ns)
}
