import scala.io._
import scala.math._
import scala.annotation.tailrec

object Day9 extends App {
  val Marker = """\((\d+)x(\d+)\)""".r
  val input  = Source.fromFile("input/day9.txt").getLines.mkString("")

  @tailrec
  def run1(remaining: String, acc: String): String = {
    Marker.findFirstMatchIn(remaining) match {
      case Some(m) =>
        val size = m.group(1).toInt
        val reps = m.group(2).toInt
        run1(m.after.toString.drop(size),
             acc + m.before + m.after.toString.take(size) * reps)
      case None => acc + remaining
    }
  }

  val result1 = run1(input, "").size
  println(result1)

  def run2(remaining: String, acc: Long): Long = {
    Marker.findFirstMatchIn(remaining) match {
      case Some(m) =>
        val size = m.group(1).toInt
        val reps = m.group(2).toInt
        run2(
            m.after.toString.drop(size),
            acc + m.before.length + run2(m.after.toString.take(size), 0) * reps
        )
      case None => acc + remaining.length
    }
  }

  println(run2(input, 0))
}
