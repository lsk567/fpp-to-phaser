package fpp.compiler.analysis

import scala.util.parsing.combinator.RegexParsers

object PeriodParser extends RegexParsers {

  override def skipWhitespace = true

  def number: Parser[Long] = "[0-9]+".r ^^ { s => s.toLong }

  def unit: Parser[String] = "(ms|us|ns)".r

  def time: Parser[Time] = ("!" ~> "period" ~> number ~ unit) ^^ {
    case value ~ "ms" => MS(value)
    case value ~ "us" => US(value)
    case value ~ "ns" => NS(value)
    case _ => throw new Exception("Invalid time unit") // should never reach here due to regex
  }

  def parse(input: String): Either[String, Time] = {
    parseAll(time, input) match {
      case Success(result, _) => Right(result)
      case _ => Left(s"Failed to parse period: $input")
    }
  }
}

object OffsetParser extends RegexParsers {

  override def skipWhitespace = true

  def number: Parser[Long] = "[0-9]+".r ^^ { s => s.toLong }

  def unit: Parser[String] = "(ms|us|ns)".r

  def time: Parser[Time] = ("!" ~> "offset" ~> number ~ unit) ^^ {
    case value ~ "ms" => MS(value)
    case value ~ "us" => US(value)
    case value ~ "ns" => NS(value)
    case _ => throw new Exception("Invalid time unit") // should never reach here due to regex
  }

  def parse(input: String): Either[String, Time] = {
    parseAll(time, input) match {
      case Success(result, _) => Right(result)
      case _ => Left(s"Failed to parse offset: $input")
    }
  }
}

object DeadlineParser extends RegexParsers {

  override def skipWhitespace = true

  /** Matches $ but discards it in the resulting string, as specified in FPP. */
  def qualifiedName: Parser[String] =
    """[A-Za-z_\$][A-Za-z0-9_\$]*(\.[A-Za-z_\$][A-Za-z0-9_\$]*)+""".r ^^ { s =>
      s.replace("$", "")
    }

  def number: Parser[Long] = "[0-9]+".r ^^ { s => s.toLong }

  def unit: Parser[String] = "(ms|us|ns)".r

  def deadline: Parser[(String, Time)] = ("!" ~> "deadline" ~> qualifiedName ~ number ~ unit) ^^ {
    case port ~ value ~ "ms" => (port, MS(value))
    case port ~ value ~ "us" => (port, US(value))
    case port ~ value ~ "ns" => (port, NS(value))
    case _ => throw new Exception("Invalid time unit") // should never reach here due to regex
  }

  def parse(input: String): Either[String, (String, Time)] = {
    parseAll(deadline, input) match {
      case Success(result, _) => Right(result)
      case _ => Left(s"Failed to parse offset: $input")
    }
  }
}
