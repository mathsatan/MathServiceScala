package com.mephistofel.mathservice.solver.mathlexer

import com.mephistofel.mathservice.solver.matherrors.{Location, MathLexerError}
import scala.util.parsing.combinator.RegexParsers

object MathLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def apply(code: String): Either[MathLexerError, List[MathToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(MathLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def tokens: Parser[List[MathToken]] = {
    phrase(rep1(comma | d | dx | lBrace | rBrace | sqrt| sin | cos |tg | ctg | arctg | arcctg | arcsin | arccos
      | ln | abs | pi | e | constant | pow | mul | add | sub | div | variable))
  }

  def constant: Parser[CONSTANT] = positioned {
    "[0-9]+(\\.[0-9]+)?".r ^^ {str => CONSTANT(BigDecimal(str))}
  }

  def variable: Parser[VARIABLE] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => VARIABLE(str) }
  }

  def add = positioned { "+" ^^ (_ => ADD()) }
  def sub = positioned { "-" ^^ (_ => SUB()) }
  def mul = positioned { "*" ^^ (_ => MUL()) }
  def div = positioned { "/" ^^ (_ => DIV()) }
  def pow = positioned { "^" ^^ (_ => POW()) }

  def d = positioned { "d(" ^^ (_ => D()) }
  def dx = positioned { ")/d" ^^ (_ => DX()) }

  def sin: Parser[SIN] = positioned { "sin" ^^ (_ => SIN()) }
  def cos: Parser[COS] = positioned { "cos" ^^ (_ => COS()) }
  def tg: Parser[TG] = positioned { "tg" ^^ (_ => TG()) }
  def ctg: Parser[CTG] = positioned { "ctg" ^^ (_ => CTG()) }
  def abs: Parser[ABS] = positioned { "abs" ^^ (_ => ABS()) }
  def sqrt: Parser[SQRT] = positioned { "sqrt" ^^ (_ => SQRT()) }

  def arctg: Parser[ARCTG] = positioned { "arctg" ^^ (_ => ARCTG()) }
  def arcctg: Parser[ARCCTG] = positioned { "arcctg" ^^ (_ => ARCCTG()) }
  def arcsin: Parser[ARCSIN] = positioned { "arcsin" ^^ (_ => ARCSIN()) }
  def arccos: Parser[ARCCOS] = positioned { "arccos" ^^ (_ => ARCCOS()) }

  def ln: Parser[LN] = positioned { "ln" ^^ (_ => LN()) }

  def pi = positioned { "pi" ^^ (_ => PI()) }
  def e = positioned { "e" ^^ (_ => E()) }
  def lBrace = positioned { "(" ^^ (_ => LBRACE()) }
  def rBrace = positioned { ")" ^^ (_ => RBRACE()) }
  def comma = positioned { "," ^^ (_ => COMMA()) }
}