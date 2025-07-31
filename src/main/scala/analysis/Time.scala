package fpp.compiler.analysis

sealed trait Time extends Ordered[Time] {
  def toNanoseconds: Long
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

case class MS(value: Long) extends Time {
  def toNanoseconds: Long = value * 1_000_000
}

case class US(value: Long) extends Time {
  def toNanoseconds: Long = value * 1_000
}

case class NS(value: Long) extends Time {
  def toNanoseconds: Long = value
}

case object ZERO extends Time {
  def toNanoseconds: Long = 0
}

object Time {
  implicit val timeOrdering: Ordering[Time] = Ordering.by(_.toNanoseconds)

  def fromNanoseconds(ns: Long): Time = NS(ns)

  // Divide a larger time value by a smaller time value and round up the result.
  def ratio(a: Time, b: Time): Long = math.ceil(a.toNanoseconds.toDouble / b.toNanoseconds).toLong
}
