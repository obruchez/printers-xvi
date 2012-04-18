package org.bruchez.printers

import scala.util.parsing.combinator.RegexParsers

object CSV extends RegexParsers {
  override val skipWhitespace = false

  def COMMA   = ","
  def DQUOTE  = "\""
  def DQUOTE2 = "\"\"" ^^ { case _ => "\"" }
  def CRLF    = "\r\n" | "\n" | "\r"
  def TXT     = "[^\",\r\n]".r
  def SPACES  = "[ \t]+".r

  def file: Parser[List[List[String]]] = repsep(record, CRLF) <~ (CRLF?)

  def record: Parser[List[String]] = repsep(field, COMMA)

  def field: Parser[String] = escaped|nonescaped

  def escaped: Parser[String] = {
    ((SPACES?)~>DQUOTE~>((TXT|COMMA|CRLF|DQUOTE2)*)<~DQUOTE<~(SPACES?)) ^^ {
      case ls => ls.mkString("")
    }
  }

  def nonescaped: Parser[String] = (TXT*) ^^ { case ls => ls.mkString("") }

  def parse(s: String) = parseAll(file, s) match {
    case Success(res, _) => res
    case e => throw new Exception(e.toString)
  }
}
