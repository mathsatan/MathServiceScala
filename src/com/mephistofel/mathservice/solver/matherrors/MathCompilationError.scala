package com.mephistofel.mathservice.solver.matherrors

sealed trait MathCompilationError{
  val msg: String
}

case class MathLexerError(location: Location, override val msg: String) extends MathCompilationError
case class MathParserError(location: Location, override val msg: String) extends MathCompilationError
case class MathLatexError(override val msg: String) extends MathCompilationError
case class MathEvaluateError(override val msg: String) extends MathCompilationError

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}

class MathParserException(msg: String) extends Exception(msg)