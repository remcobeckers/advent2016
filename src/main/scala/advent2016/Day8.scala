import scala.io._
import scala.math._
import scala.annotation.tailrec

object Day8 extends App {
  val Rect         = """rect (\d+)x(\d+)""".r
  val RotateRow    = """rotate row y=(\d+) by (\d+)""".r
  val RotateColumn = """rotate column x=(\d+) by (\d+)""".r

  val input = Source.fromFile("input/day8.txt").getLines

  var display = Array.fill(6, 50)(false)

  def show = {
    println(display.map { row =>
      row.map(if (_) "#" else ".").mkString
    }.mkString("\n"))
    println("")
  }

  input.foreach {
    case Rect(x, y) =>
      for {
        row    <- (0 until y.toInt)
        column <- (0 until x.toInt)
      } display(row)(column) = true
    case RotateRow(y, d) =>
      val row   = display(y.toInt)
      val shift = row.size - (d.toInt % row.size)
      display(y.toInt) = row.drop(shift) ++ row.take(shift)
    case RotateColumn(x, d) =>
      val currentValues = display.map(row => row(x.toInt))
      currentValues.zipWithIndex.foreach {
        case (value, i) =>
          display((i + d.toInt) % display.size)(x.toInt) = value
      }
  }
  println(display.map(_.count(identity)).sum)
  show
}
