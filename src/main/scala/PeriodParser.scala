package fpp.compiler.analysis

import scala.util.parsing.combinator.RegexParsers

object PeriodParser extends RegexParsers {

  override def skipWhitespace = true

  def number: Parser[BigInt] = "[0-9]+".r ^^ { s => BigInt(s) }

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

  def number: Parser[BigInt] = "[0-9]+".r ^^ { s => BigInt(s) }

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
