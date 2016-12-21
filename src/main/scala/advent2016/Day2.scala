/*
 * Copyright 2016 Remco Beckers
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import scala.io._
import scala.math._
import scala.annotation.tailrec

object Day2 extends App {
  val input = Source.fromFile("input/day2.txt").getLines.toList.map(_.trim)
  val keypad = // Vector("123", "456", "789")
  Vector("  1  ", " 234 ", "56789", " ABC ", "  D  ").map(_.toVector)

  def lookup(c: (Int, Int)) = {
    Some(keypad)
      .filter(_ => c._1 >= 0 && c._1 < keypad.size)
      .map(pad => pad(c._1))
      .filter(row => c._2 >= 0 && c._2 < row.size)
      .map(row => row(c._2))
      .getOrElse(' ')
  }

  @tailrec
  def findNum(previous: (Int, Int), line: List[Char]): (Int, Int) = {
    if (line == Nil) previous
    else {
      val (newPos, rest) = line match {
        case 'U' :: tail => ((previous._1 - 1, previous._2), tail)
        case 'R' :: tail => ((previous._1, previous._2 + 1), tail)
        case 'L' :: tail => ((previous._1, previous._2 - 1), tail)
        case 'D' :: tail => ((previous._1 + 1, previous._2), tail)
      }
      findNum(if (lookup(newPos) == ' ') previous else newPos, rest)
    }
  }

  val result = input
    .foldLeft((0, 2) :: Nil)(
        (acc, line) => findNum(acc.head, line.toList) :: acc
    )
    .reverse
    .tail
    .map(lookup)
    .mkString
  println(result)
}
