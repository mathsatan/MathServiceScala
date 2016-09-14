package com.mephistofel.mathservice.solver.mathparser

import com.mephistofel.mathservice.solver.matherrors.{Location, MathParserError, MathParserException}
import com.mephistofel.mathservice.solver.mathlexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

/**
  * Created by max_2 on 8/12/2016.
  */
object MathParser extends Parsers {
  override type Elem = MathToken

  class MathTokenReader(tokens: Seq[MathToken]) extends Reader[MathToken] {
    override def first: MathToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[MathToken] = new MathTokenReader(tokens.tail)
  }

  def apply(tokens: Seq[MathToken]): Either[MathParserError, MathAST] = {
    val reader = new MathTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(MathParserError(Location(next.pos.line, next.pos.column), "MathParser: " + msg))
      case Success(result, next) => Right(result)
    }
  }

  def program: Parser[MathAST] = positioned {
      phrase(block)
  }
  def block: Parser[MathAST] = positioned {
    rep1(expression) ^^ (list => list.reduceRight(To))
  }

  def factor: Parser[MathAST] = positioned {
    constant|variable|op1|LBRACE()~expression~RBRACE()^^{case _~e~_ => e}
  }

  def power: Parser[MathAST] = positioned {
    term~POW()~term^^{case x~POW()~y => Pow(x, y)}  //factor~POW()~factor^^...
  }

  def term: Parser[MathAST] = positioned {
    factor~rep(MUL()~factor | DIV()~factor)^^{
      case f~list => list.foldLeft(f){
        case (x, MUL()~y) => Mul(x,y)
        case (x, DIV()~y) => Div(x,y)
      }
    }
  }

  def expression: Parser[MathAST] = positioned {
    val v = power|term
    v~rep(ADD()~v | SUB()~v)^^{ //term~rep(ADD()~term | SUB()~term)^^{
      case f~list => list.foldLeft(f){
        case (x, ADD()~y) => Add(x,y)
        case (x, SUB()~y) => Sub(x,y)
      }
    }
  }

  def op1: Parser[MathAST] = positioned {
    val _d = D()~expression~DX()~LBRACE()~variable~RBRACE() ^^{
      case D()~f~DX()~_~dx~_ => dx match {
        case Variable(name) => Differentiate(f, Variable(name))
        case _ => throw new MathParserException("MathParser: Unknown variable type in df/dx")
      }
    }
    val _sin = SIN()~LBRACE()~expression~RBRACE() ^^{
      case SIN()~_~e~_ => Sin(e)
    }
    val _cos = COS()~LBRACE()~expression~RBRACE() ^^{
      case COS()~_~e~_ => Cos(e)
    }
    val _ln = LN()~LBRACE()~expression~RBRACE() ^^{
      case LN()~_~e~_ => Ln(e)
    }
    val _tg = TG()~LBRACE()~expression~RBRACE() ^^{
      case TG()~_~e~_ => Tg(e)
    }
    val _ctg = CTG()~LBRACE()~expression~RBRACE() ^^{
      case CTG()~_~e~_ => Ctg(e)
    }
    val _abs = ABS()~LBRACE()~expression~RBRACE() ^^{
      case ABS()~_~e~_ => Abs(e)
    }
    val _arcsin = ARCSIN()~LBRACE()~expression~RBRACE() ^^{
      case ARCSIN()~_~e~_ => Arcsin(e)
    }
    val _arccos = ARCCOS()~LBRACE()~expression~RBRACE() ^^{
      case ARCCOS()~_~e~_ => Arccos(e)
    }

    val _arctg = ARCTG()~LBRACE()~expression~RBRACE() ^^{
      case ARCTG()~_~e~_ => Arctg(e)
    }
    val _arcctg = ARCCTG()~LBRACE()~expression~RBRACE() ^^{
      case ARCCTG()~_~e~_ => Arcctg(e)
    }
    val _sqrt = SQRT()~LBRACE()~expression~RBRACE() ^^{
      case SQRT()~_~e~_ => Sqrt(e)
    }
    val _usub = SUB()~LBRACE()~expression~RBRACE() ^^{
      case SUB()~_~e~_ => Usub(e)
    }|SUB()~(power|term) ^^{
      case SUB()~e => Usub(e)
    }

    _d |_sin  | _cos | _ln |_tg | _ctg | _abs | _usub | _arcsin | _arcsin | _arctg | _arcctg | _sqrt
  }

  private def variable: Parser[MathAST] = positioned {
    accept("variable", { case VARIABLE(name) => Variable(name) })
  }

  private def constant: Parser[MathAST] = positioned {
    accept("string constant", { case CONSTANT(value) => Constant(value) case PI() => Pi case E() => Exponenta})
  }
}