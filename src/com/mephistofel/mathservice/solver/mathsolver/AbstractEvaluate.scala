package com.mephistofel.mathservice.solver.mathsolver

import com.mephistofel.mathservice.solver.matherrors.{MathCompilationError, MathEvaluateError}
import com.mephistofel.mathservice.solver.mathparser._

import scala.util.{Failure, Success, Try}

/**
  * Created by max_2 on 8/20/2016.
  */
object AbstractEvaluate {
  def apply(e: MathAST): Either[MathCompilationError, (MathAST, List[String])] = {
    Try[MathAST](evaluate(e)) match {
      case Failure(msg) => Left(MathEvaluateError(msg.getMessage))
      case Success(result) => Right((result, variablesInMathFunc(result)))
    }
  }

  def variablesInMathFunc(e: MathAST): List[String] = {
    val vars = new scala.collection.mutable.ListBuffer[String]()
    def g(e: MathAST): Unit =
      e match {
        case Variable(v) if !vars.contains(v) => vars += v
        case s: SingleToken => g(s.a)
        case d: DoubleToken => g(d.a); g(d.b)
        case _ =>
      }
    g(e)
    vars.toList
  }

  private def simplifyDiv(a: MathAST, b: MathAST): MathAST =
    b match {
      case Constant(_b) =>
        if(_b.compare(0) == 0) throw new AbstractEvaluateException("Simplify: Divide by zero")
        else a match {
          case Constant(_a) => Constant(_a / _b)
          case _a: MathAST => if(_b.compare(1) == 0) _a else Div(_a, Constant(_b))
        }
      case _b: MathAST => a match {
        case _a: Constant => if(_a.value.compare(0) == 0) _a else Div(_a, _b)
        case _a: MathAST => if (_a == _b) Constant(BigDecimal(1)) else Div(_a, _b)
      }
    }

  private def simplifyMul(a: MathAST, b: MathAST): MathAST =
    a match {
      case Constant(_a) => if(_a.compare(1) == 0) b else if(_a.compare(0) == 0) Constant(_a) else b match {
        case Constant(_b) => Constant(_a * _b)
        case _b: MathAST => Mul(Constant(_a), _b)
      }
      case _ => b match {
        case _b: Constant => if(_b.value.compare(1) == 0) a else if(_b.value.compare(0) == 0) b else Mul(a, _b)
        case _b: MathAST => if(_b == a) Pow(a, Constant(BigDecimal(2))) else Mul(a, _b)
      }
    }

  private def simplifySub(a: MathAST, b: MathAST): MathAST =
    a match {
      case Constant(_a) => if(_a.compare(0) == 0) b else b match {
        case Constant(_b) => Constant(_a - _b)
        case _b: MathAST => Sub(Constant(_a), _b)
      }
      case _a: MathAST => b match {
        case _b: Constant => if(_b.value.compare(0) == 0) _a else Sub(_a, _b)
        case _b: MathAST => Sub(_a, _b)
      }
    }

  private def simplifyAdd(a: MathAST, b: MathAST): MathAST =
    a match {
      case Constant(_a) => if(_a.compare(0) == 0) b else b match {
        case Constant(_b) => Constant(_a + _b)
        case _b: MathAST => Add(Constant(_a), _b)
      }
      case _a: MathAST => b match {
        case _b: Constant => if(_b.value.compare(0) == 0) _a else Add(_a, _b)
        case _b: MathAST => Add(_a, _b)
      }
    }

  private def simplifyPow(a: MathAST, b: MathAST): MathAST = {
    a match {
      case Constant(_a) =>
        b match {
          case Constant(_b) =>
            if (a == Constant(BigDecimal(0)) && b == Constant(BigDecimal(0)))
              throw new AbstractEvaluateException("Simplify: Undefined 0^0")
            else Constant(Math.pow(_a.toDouble, _b.toDouble))
          case _ => Pow(a, b)
        }
      case _ => b match {
        case Constant(c) => if (c == 1) a else Pow(a, b)
        case _ => Pow(a, b)
      }
    }
  }

  private def simplifySin(a: MathAST): MathAST = {
    if (a == Constant(BigDecimal(0)) || a == Mul(Constant(BigDecimal(2)), Pi) || a == Mul(Pi, Constant(BigDecimal(2))) || a == Pi) Constant(BigDecimal(0))
    else a match {
      case Constant(_a) => Constant(Math.sin(_a.toDouble))
      case _ => Sin(a)
    }
  }
  private def simplifyCos(a: MathAST): MathAST = {
    if (a == Constant(BigDecimal(0)) || a == Mul(Constant(BigDecimal(2)), Pi)) Constant(BigDecimal(1))
    if (a == Div(Mul(Constant(BigDecimal(3)), Pi), Constant(BigDecimal(2)))) Constant(BigDecimal(0))
    if (a == Div(Mul(Pi, Constant(BigDecimal(3))), Constant(BigDecimal(2)))) Constant(BigDecimal(0))
    else a match {
      case Constant(_a) => Constant(Math.cos(_a.toDouble))
      case _ => Cos(a)
    }
  }

  private def simplifyTg(a: MathAST): MathAST = {
    simplifyCos(a) match {
      case Constant(c) if c == 0 => throw new AbstractEvaluateException("Simplify: Tg undefined")
      case _ => a match {
        case Constant(c) => Constant(Math.tan(c.toDouble))
        case _ => Tg(a)
      }
    }
  }
  private def simplifyCtg(a: MathAST): MathAST = {
    simplifySin(a) match {
      case Constant(c) if c == 0 => throw new AbstractEvaluateException("Simplify: Ctg undefined")
      case _ => a match {
        case Constant(c) => Constant(1.0d / Math.tan(c.toDouble))
        case _ => Ctg(a)
      }
    }
  }

  private def simplifyLn(a: MathAST): MathAST = a match {
    case Usub(_) => throw new AbstractEvaluateException("Simplify: Negative argument of ln(x)")
    case Constant(c) => if (c.compare(0) == 0) throw new AbstractEvaluateException("Simplify: Zero argument of ln(x), so ln(0) = -\\infty")
      else if (c.compare(0) < 0) throw new AbstractEvaluateException("Simplify: Negative argument of ln(x)")
      else Constant(Math.log(c.toDouble))
    case Exponenta => Constant(BigDecimal(1))
    case _ => Ln(a)
  }
  private def simplifyAbs(a: MathAST): MathAST = a match {
    case Usub(e) => e
    case Constant(c) => Constant(c.abs)
    case _ => Abs(a)
  }
  private def simplifySqrt(a: MathAST): MathAST = a match {
    case Usub(_) => throw new AbstractEvaluateException("Simplify: Negative argument of sqrt(x)")
    case c: Constant =>
      if (c.value.compare(0) < 0) throw new AbstractEvaluateException("Simplify: Negative argument of sqrt(x)")
      else Constant(BigDecimal(Math.sqrt(c.value.toDouble)))
    case _ => Sqrt(a)
  }

  private def simplifyUsub(a: MathAST): MathAST = a match {
    case Usub(_a) => _a
    case Constant(_a) => Constant(_a * BigDecimal(-1))
    case _ => Usub(a)
  }

  def evaluate(e: MathAST): MathAST = e match {
    /*case Pi => Constant(BigDecimal(3.1415926535))
    case Exponenta => Constant(BigDecimal(2.7182818284))*/

    case Differentiate(f, dx) => evaluate(Derivative(evaluate(f))(dx.name))

    case e: SingleToken =>
      val arg = evaluate(e.a)
      e match {
        case Sin(_) => simplifySin(arg)
        case Cos(_) => simplifyCos(arg)
        case Tg(_) => simplifyTg(arg)
        case Ctg(_) => simplifyCtg(arg)
        case Arcsin(_) => Arcsin(arg)
        case Arccos(_) => Arccos(arg)
        case Arctg(_) => Arctg(arg)
        case Arcctg(_) => Arcctg(arg)

        case Abs(_) => simplifyAbs(arg)
        case Sqrt(_) => simplifySqrt(arg)
        case Ln(_) => simplifyLn(arg)
        case Usub(_) => simplifyUsub(arg)
        case _ => throw new AbstractEvaluateException("Simplify: Unknown unary operator")
      }

    case e: DoubleToken =>
     val left = evaluate(e.a)
     val right = evaluate(e.b)
      e match {
        case Add(_,_) => simplifyAdd(left, right)
        case Sub(_,_) => simplifySub(left, right)
        case Mul(_,_) => simplifyMul(left, right)
        case Div(_,_) => simplifyDiv(left, right)
        case Pow(_,_) => simplifyPow(left, right)
        case _ => throw new AbstractEvaluateException("Simplify: Unknown binary operator")
      }
    case e: LeafToken => e

    case _ => throw new AbstractEvaluateException("Simplify: Incorrect data")
  }
}