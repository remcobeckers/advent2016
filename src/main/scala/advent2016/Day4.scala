import scala.io._
import scala.math._
import scala.annotation.tailrec

object Day4 extends App {
  val Split = """([a-z\-]+)-(\d+)\[([a-z]+)\]""".r
  val input = Source
    .fromFile("input/day4.txt")
    .getLines
    .flatMap {
      case Split(name, sectorId, chk) =>
        val calcCheckSum = name
          .replace("-", "")
          .toVector
          .groupBy(identity)
          .toVector
          .map { case (k, v) => k -> v.size }
          .sortWith {
            case ((chr1, lst1), (chr2, lst2)) =>
              if (lst1 > lst2) true
              else if (lst1 < lst2) false
              else chr1 < chr2
          }
          .map(_._1)
          .mkString
          .take(5)
        if (calcCheckSum == chk) Some(sectorId.toLong, name)
        else None
    }
    .toVector

  val result1 = input.map(_._1).sum

  val result2 = input.map {
    case (rotBy, name) =>
      rotBy -> name.map {
        case '-' => '-'
        case chr => (((chr - 97 + rotBy) % 26) + 97).toChar
      }.mkString
  }

  println(result1)
  println(result2.find(_._2.contains("pole")))
}
