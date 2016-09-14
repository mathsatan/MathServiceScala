package com.mephistofel.mathservice.solver.mathlatex
import com.mephistofel.mathservice.solver.matherrors.MathLatexError
import com.mephistofel.mathservice.solver.mathparser._

import scala.util.{Failure, Success, Try}

object MathLatex {

  def apply(tree: MathAST): Either[MathLatexError, String] =
    Try(eval(tree)) match {
      case Success(str) => Right(eval(tree))
      case Failure(e) => Left(MathLatexError(e.getMessage))
    }

  def eval(tree: MathAST): String = tree match{
    case Differentiate(a, b) => "\\frac{\\partial \\left(" + eval(a) + "\\right)}{\\partial " + eval(b) + "}"
    case Pow(a, b) => "{" + eval(a) + "}^{" + eval(b) + "}"
    case Add(a, b) => "\\left(" + eval(a) + " + " + eval(b) + "\\right)"
    case Sub(a, b) => "\\left(" + eval(a) + " - " + eval(b) + "\\right)"
    case Mul(a, b) => eval(a) + "\\cdot " + eval(b)
    case Div(a, b) => "\\dfrac{" + eval(a) + "}{" + eval(b) + "}"
    case Sin(a) => "sin\\left(" + eval(a) + "\\right)"
    case Cos(a) => "cos\\left(" + eval(a) + "\\right)"

    case Arcsin(a) => "arcsin\\left(" + eval(a) + "\\right)"
    case Arccos(a) => "arccos\\left(" + eval(a) + "\\right)"
    case Arctg(a) => "arctg\\left(" + eval(a) + "\\right)"
    case Arcctg(a) => "arcctg\\left(" + eval(a) + "\\right)"

    case Tg(a) => "tg\\left(" + eval(a) + "\\right)"
    case Ctg(a) => "ctg\\left(" + eval(a) + "\\right)"
    case Abs(a) => "|" + eval(a) + "|"
    case Sqrt(a) => "\\sqrt{" + eval(a) + "}"
    case Ln(a) => "ln\\left(" + eval(a) + "\\right)"
    case Usub(a) => "-\\left(" + eval(a) + "\\right)"
    case Variable(a) => a
    case Constant(a) => a.toString()
    case Pi => "\\pi "
    case Exponenta => " e "

    case e => throw new LatexException("Latex: Unknown expression: " + e);
  }
}

