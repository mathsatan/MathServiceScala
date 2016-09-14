package com.mephistofel.mathservice.solver.mathplot

import java.awt.{Color, Graphics2D}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.io.File
import java.util.Date

import com.mephistofel.mathservice.solver.matherrors.MathEvaluateError
import com.mephistofel.mathservice.solver.mathparser._
import com.mephistofel.mathservice.solver.mathsolver.AbstractEvaluateException

import scala.util.{Failure, Random, Success, Try}

/**
  * Created by max_2 on 9/1/2016.
  */


object MathPlot{
  private val w = 480
  private val h = 320
  private val PLOT_PATH = getImagesPath

  private var canvas: BufferedImage = _
  private var g: Graphics2D = _

  private val indent = 10
  private val xRange: Double = (w - 2*indent-5)/2
  private val yRange: Double = (xRange*h-5)/w
  private val scale = 30

  def getImagesPath: String = {
    val f = this.getClass.getClassLoader.getResource("")
    new File(f.toURI).getAbsolutePath + "/imgs/"
  }

  def apply(func: MathAST, varList: List[String]): Either[MathEvaluateError, Option[String]] = {
    Try[Option[String]](start(func, varList)) match {
      case Failure(msg) => Left(MathEvaluateError(msg.getMessage))
      case Success(result) => Right(result)
    }
  }

  def f(expression: MathAST)(implicit variables: Map[String, Double]): Double = expression match {
    case Variable(v) => variables(v)
    case Pow(a, b) => if (a == Constant(BigDecimal(0)) && b == Constant(BigDecimal(0)))
      throw new AbstractEvaluateException("MathPlot: Undefined 0^0") else Math.pow(f(a), f(b))
    case Add(a, b) => f(a) + f(b)
    case Sub(a, b) => f(a) + f(b)
    case Mul(a, b) => f(a) * f(b)
    case Div(a, b) => val v = f(b)
      if (v == 0.0d) throw new AbstractEvaluateException("MathPlot: Divide by zero")
      else f(a) / v

    case Sin(a) => Math.sin(f(a))
    case Cos(a) => Math.cos(f(a))
    case Tg(a) =>  Math.tan(f(a))
    case Ctg(a) => 1.0d/Math.tan(f(a))

    case Arcsin(a) => Math.asin(f(a))
    case Arccos(a) => Math.acos(f(a))
    case Arctg(a) => Math.atan(f(a))
    case Arcctg(a) => 1.0d/Math.atan(f(a))

    case Abs(a) => Math.abs(f(a))
    case Sqrt(a) => val v = f(a)
      if (v == 0.0d || v < 0.0d) throw new AbstractEvaluateException("MathPlot: Negative sqrt argument")
      else Math.sqrt(v)
    case Ln(a) => val v = f(a)
      if (v == 0.0d || v < 0.0d) throw new AbstractEvaluateException("MathPlot: Wrong ln argument")
      else Math.log(v)
    case Usub(a) => -f(a)
    case Constant(a) => a.toDouble
    case Pi => Math.PI
    case Exponenta => Math.E
    case _ => throw new AbstractEvaluateException("MathPlot: Wrong input data")
  }

  def start(func: MathAST, variables: List[String]): Option[String] = {
    if (variables.length != 1) return None
    canvas = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    g = canvas.createGraphics()

    clearBackground
    drawAxes
    //drawFrame

    val delta = 0.05d
    g.setColor(Color.RED)
    var x0: Double = -xRange
    val m = Map(variables.head -> -xRange)
    var y0: Double = f(func)(m)
    var x1: Double = -xRange + delta
    var y1: Double = 0.0d

    while (x1 < xRange) {
      y1 = f(func)(m + (variables.head -> x1))
      drawLineLogic((scale*x0).toInt, (scale*y0).toInt, (scale*x1).toInt, (scale*y1).toInt)
      x0 = x1
      y0 = y1
      x1 += delta
    }
    g.dispose()
    Some(saveFile)
  }

  private def clearBackground: Unit = {
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, w, h)
  }

  private def createFlipped(image: BufferedImage): BufferedImage = {
    val at: AffineTransform = new AffineTransform()
    at.concatenate(AffineTransform.getScaleInstance(1, -1))
    at.concatenate(AffineTransform.getTranslateInstance(0, -image.getHeight()))
    createTransformed(image, at)
  }
  private def createTransformed(image: BufferedImage, at: AffineTransform): BufferedImage = {
    val newImage: BufferedImage = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_INT_ARGB)
    val g = newImage.createGraphics()
    g.transform(at)
    g.drawImage(image, 0, 0, null)
    g.dispose()
    newImage
  }

  private def drawAxes: Unit = {
    g.setColor(Color.BLACK)
    drawLineLogic(-xRange.toInt, 0, xRange.toInt, 0)
    drawLineLogic(0, -yRange.toInt, 0, yRange.toInt)
    var x = w/2
    var y = h/2
    while (x <= w-indent){
      g.drawLine(x, y + 3, x, y)
      x += scale
    }
    x = w/2
    y = h/2
    while (y <= h-indent){
      g.drawLine(x , y, x + 3, y)
      y += scale
    }
  }

  private def drawLineLogic(_x0: Int, _y0: Int, _x1: Int, _y1: Int): Unit = {
    if (Math.abs(_y0) < (h/2) && Math.abs(_y1) < (h/2) && Math.abs(_x0) < (w/2 - indent) && Math.abs(_x1) < (w/2 - indent))
      g.drawLine(_x0 + w / 2, _y0 + h / 2, _x1 + w / 2, _y1 + h / 2)
  }

  private def deleteFiles(n: Int): Unit = {
    val folder: File = new File(PLOT_PATH)
    val listOfFiles: Array[File] = folder.listFiles()
    listOfFiles.foreach{file =>
      val diff = new Date().getTime - file.lastModified()
      if (diff > n * 86400000/*24 * 60 * 60 * 1000*/) {
        file.delete()
      }
    }
  }

  private def saveFile: String = {
    if (Random.nextInt(10) < 6) deleteFiles(1)
    val name = s"math-deque.rhcloud.com_${Random.alphanumeric take 10 mkString("")}.png"
    val filePath = PLOT_PATH + name
    javax.imageio.ImageIO.write(createFlipped(canvas), "png", new java.io.File(filePath))
    name
  }

  /*private def drawFrame: Unit = {
    g.setColor(Color.BLACK)
    g.drawRect(indent, 4*indent, w-2*indent, h-2*indent)
  }*/
}