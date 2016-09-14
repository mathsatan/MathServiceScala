package com.mephistofel.mathservice.solver.mathsolver

import com.mephistofel.mathservice.solver.mathparser._

/**
  * Created by max_2 on 8/20/2016.
  */
object Derivative {
  def apply(e: MathAST)(implicit dx: String): MathAST = differentiate(e)

  def differentiate(f: MathAST)(implicit dx: String): MathAST = f match {
    case Differentiate(_f, _dx) => differentiate(_f)(_dx.name)

    case Add(a, b) => Add(differentiate(a), differentiate(b))
    case Sub(a, b) => Sub(differentiate(a), differentiate(b))
    case Div(a, b) => Div(Sub(Mul(differentiate(a), b), Mul(a, differentiate(b))), Pow(b, Constant(BigDecimal(2))))

    case Pow(a, b) => {
      val u = isDependsOnVar(a)
      val v = isDependsOnVar(b)
      if (u && !v) Mul(Mul(Pow(a, Sub(b, Constant(BigDecimal(1)))), differentiate(a)), b) // u(x)^c, c=const
      else if (!u && v){  // c^u(x), c=const
        Mul(Mul(Pow(a, b), differentiate(b)), Ln(a))
      }else if(!u && !v){
        Constant(BigDecimal(0))
      }else Mul(Pow(a, Sub(b, Constant(BigDecimal(1)))), Add(Mul(b, differentiate(a)), Mul(Mul(a, Ln(a)), differentiate(b))))
    }
    case Mul(a, b) => {
      val u = isDependsOnVar(a)
      val v = isDependsOnVar(b)
      if (u && !v) {
        Mul(differentiate(a), b)
      } else if (!u && v){  // c^u(x), c=const
        Mul(a, differentiate(b))
      }else if (!u && !v){// c^c, c=const
        Constant(BigDecimal(0))
      }else Add(Mul(differentiate(a), b), Mul(a, differentiate(b)))
    }

    case e: SingleToken =>
      val d = e match {
        case Sin(x) => Cos(x)
        case Cos(x) => Usub(Sin(x))
        case Tg(x) => Div(Constant(BigDecimal(1)), Pow(Cos(x), Constant(BigDecimal(2))))
        case Ctg(x) => Usub(Div(Constant(BigDecimal(1)), Pow(Sin(x), Constant(BigDecimal(2)))))
        case Abs(x) => Div(x, Abs(x))
        case Ln(x) => Div(Constant(BigDecimal(1)), x)
        case Sqrt(x) => Div(Constant(BigDecimal(1)), Mul(Constant(BigDecimal(2)), Sqrt(x)))
        case Usub(x) => Usub(differentiate(x))
        case Arcsin(x) => Div(Constant(BigDecimal(1)), Sqrt(Sub(Constant(BigDecimal(1)), Pow(x, Constant(BigDecimal(2))))))
        case Arccos(x) => Usub(Div(Constant(BigDecimal(1)), Sqrt(Sub(Constant(BigDecimal(1)), Pow(x, Constant(BigDecimal(2)))))))
        case Arctg(x) => Div(Constant(BigDecimal(1)), Sub(Constant(BigDecimal(1)), Pow(x, Constant(BigDecimal(2)))))
        case Arcctg(x) => Usub(Div(Constant(BigDecimal(1)), Sub(Constant(BigDecimal(1)), Pow(x, Constant(BigDecimal(2))))))
        case _ => throw new AbstractEvaluateException("Differentiate: Unknown unary operator")
      }
      if (isLeaf(e.a)) d else Mul(d, differentiate(e.a))

    case Variable(a) => if (a == dx) Constant(BigDecimal(1)) else Constant(BigDecimal(0))
    case Constant(a) => Constant(BigDecimal(0))
    case Pi | Exponenta => Constant(BigDecimal(0))
    case _ => throw new AbstractEvaluateException("Differentiate: Wrong input data")
  }

  private def isLeaf(e: MathAST): Boolean = e match {
    case Variable(_) | Constant(_) => true
    case  Pi | Exponenta => true
    case _ => false
  }

  private def isDependsOnVar(tree: MathAST)(implicit dx: String): Boolean = tree match{
    case e: DoubleToken => (e.a match {
      case Variable(name) => if(name == dx) true else false
      case _ => isDependsOnVar(e.a)
    })||(e.b match {
      case Variable(name) => if(name == dx) true else false
      case _ => isDependsOnVar(e.b)
    })
    case e: SingleToken => isDependsOnVar(e.a)
    case Variable(name) => if(name == dx) true else false
    case _ => false
  }
}