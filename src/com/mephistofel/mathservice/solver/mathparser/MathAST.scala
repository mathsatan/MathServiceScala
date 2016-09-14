package com.mephistofel.mathservice.solver.mathparser
import scala.util.parsing.input.Positional

sealed trait MathAST extends Positional
sealed trait SingleToken extends MathAST{
  val a: MathAST
}
sealed trait DoubleToken extends MathAST{
  val a: MathAST
  val b: MathAST
}
sealed trait LeafToken extends MathAST

case class To(t: MathAST, t2: MathAST) extends MathAST

case object Pi extends LeafToken
case object Exponenta extends LeafToken

case class Sin(override val a: MathAST) extends SingleToken
case class Cos(override val a: MathAST) extends SingleToken
case class Tg(override val a: MathAST) extends SingleToken
case class Ctg(override val a: MathAST) extends SingleToken
case class Ln(override val a: MathAST) extends SingleToken
case class Abs(override val a: MathAST) extends SingleToken
case class Usub(override val a: MathAST) extends SingleToken
case class Arcsin(override val a: MathAST) extends SingleToken
case class Arccos(override val a: MathAST) extends SingleToken
case class Arctg(override val a: MathAST) extends SingleToken
case class Arcctg(override val a: MathAST) extends SingleToken
case class Sqrt(override val a: MathAST) extends SingleToken

case class Mul(override val a: MathAST, override val b: MathAST) extends DoubleToken
case class Add(override val a: MathAST, override val b: MathAST) extends DoubleToken
case class Sub(override val a: MathAST, override val b: MathAST) extends DoubleToken
case class Div(override val a: MathAST, override val b: MathAST) extends DoubleToken
case class Pow(override val a: MathAST, override val b: MathAST) extends DoubleToken
case class Differentiate(f: MathAST, dx: Variable) extends MathAST

case class Variable(name: String) extends LeafToken
case class Constant(value: BigDecimal) extends LeafToken
