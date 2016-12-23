import scala.io._
import scala.math._
import scala.annotation.tailrec

object Day6 extends App {
  val input = Source.fromFile("input/day6.txt").getLines.toVector.transpose

  val result = input.map { column =>
    column.groupBy(identity).maxBy(_._2.size)._1
  }.mkString

  println(result)

  val result2 = input.map { column =>
    column.groupBy(identity).minBy(_._2.size)._1
  }.mkString
  println(result2)
}
