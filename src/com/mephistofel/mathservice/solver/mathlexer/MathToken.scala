package com.mephistofel.mathservice.solver.mathlexer

import scala.util.parsing.input.Positional

sealed trait MathToken extends Positional

case class CONSTANT(digit: BigDecimal) extends MathToken
case class VARIABLE(str: String) extends MathToken

case class SIN() extends MathToken
case class COS() extends MathToken
case class TG() extends MathToken
case class CTG() extends MathToken
case class LN() extends MathToken
case class SQRT() extends MathToken
case class ARCCOS() extends MathToken
case class ARCSIN() extends MathToken
case class ARCTG() extends MathToken
case class ARCCTG() extends MathToken

case class ADD() extends MathToken
case class MUL() extends MathToken
case class SUB() extends MathToken
case class DIV() extends MathToken
case class POW() extends MathToken
case class ABS() extends MathToken

case class LBRACE() extends MathToken
case class RBRACE() extends MathToken
case class COMMA() extends MathToken
case class D() extends MathToken
case class DX() extends MathToken
case class PI() extends MathToken
case class E() extends MathToken

