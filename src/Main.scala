import com.mephistofel.mathservice.solver.matherrors.MathCompilationError
import com.mephistofel.mathservice.solver.mathlatex.MathLatex
import com.mephistofel.mathservice.solver.mathlexer._
import com.mephistofel.mathservice.solver.mathparser._
import com.mephistofel.mathservice.solver.mathsolver.AbstractEvaluate

object Main {

  def main(args: Array[String]) {
    go("d(sin(x^2))/d(x)") match {
      case Left(e: MathCompilationError) => print(e.msg)
      case Right(result) => print(result._2)
    }
  }

  def go(code: String): Either[MathCompilationError, (String, String, (MathAST, List[String]))] = {
    for {
      tokens <- MathLexer(code).right
      ast <- MathParser(tokens).right
      inputMathFunc <- MathLatex(ast).right
      resultMathAst <- AbstractEvaluate(ast).right
      resultMathFunc <- MathLatex(resultMathAst._1).right
    } yield (inputMathFunc, resultMathFunc, resultMathAst)
  }
}