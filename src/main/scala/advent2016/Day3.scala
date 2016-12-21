import scala.io._
import scala.math._
import scala.annotation.tailrec

object Day3 extends App {
  val Triangle = """\s*(\d+)\s+(\d+)\s+(\d+)""".r
  val input = Source
    .fromFile("input/day3.txt")
    .getLines
    .map(_.trim)
    .map {
      case Triangle(a, b, c) => Vector(a, b, c).map(_.toInt)
    }
    .toVector

  val input2 = input.transpose.flatten.grouped(3)

  def isPossible(sides: Vector[Int]) =
    sides.combinations(2).map(_.sum).forall(sum => sides.forall(_ < sum))

  // val result = input.count(isPossible)
  // println(result)

  val result2 = input2.count(isPossible)
  println(result2)
}
