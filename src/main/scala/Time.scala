package fpp.compiler.analysis

sealed trait Time extends Ordered[Time] {
  def toNanoseconds: BigInt
  def toNS: Time = Time.fromNanoseconds(this.toNanoseconds)

  def +(that: Time): Time = Time.fromNanoseconds(this.toNanoseconds + that.toNanoseconds)
  def -(that: Time): Time = Time.fromNanoseconds(this.toNanoseconds - that.toNanoseconds)

  override def equals(obj: Any): Boolean = obj match {
    case that: Time => this.toNanoseconds == that.toNanoseconds
    case _ => false
  }
  
  override def hashCode(): Int = toNanoseconds.hashCode()

  override def compare(that: Time): Int =
    this.toNanoseconds.compare(that.toNanoseconds)
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

case object ZERO extends Time {
  def toNanoseconds: BigInt = 0
}

object Time {
  implicit val timeOrdering: Ordering[Time] = Ordering.by(_.toNanoseconds)

  def fromNanoseconds(ns: BigInt): Time = NS(ns)
}
