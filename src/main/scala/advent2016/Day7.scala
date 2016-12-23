import scala.io._
import scala.math._
import scala.annotation.tailrec

object Day7 extends App {
  val Split = """(?:([a-z]+)|\[([a-z]+)\])""".r

  val input = Source.fromFile("input/day7.txt").getLines

  def containsAnagram4(value: String) = {
    value
      .sliding(4)
      .find { four =>
        val (start, end) = four.splitAt(2)
        start == end.reverse && start.distinct.size == 2
      }
      .isDefined
  }
  //
  // val result = input.count { value =>
  //   val (names, brackets) =
  //     Split.findAllMatchIn(value).partition(_.group(1) != null)
  //   !brackets.toIterator.map(_.group(2)).find(containsAnagram4).isDefined &&
  //   names.toIterator.map(_.group(1)).find(containsAnagram4).isDefined
  // }
  //
  // println(result)

  def isTriplet(value: String)     = value(0) == value(2) && value(0) != value(1)
  def invertTriplet(value: String) = s"${ value(1) }${ value(0) }${ value(1) }"

  val result2 = input.count { value =>
    val (nameMatches, bracketMatches) =
      Split.findAllMatchIn(value).partition(_.group(1) != null)

    val names    = nameMatches.map(_.group(1))
    val brackets = bracketMatches.map(_.group(2)).toSet

    names
      .flatMap(_.sliding(3, 1))
      .filter(isTriplet _)
      .map(invertTriplet _)
      .find(inverted => brackets.exists(_.contains(inverted)))
      .isDefined
  }

  println(result2)
}
