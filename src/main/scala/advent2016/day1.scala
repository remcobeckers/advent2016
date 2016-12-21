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

object Day1 extends App {
  val R = """\s*R(\d+)""".r
  val L = """\s*L(\d+)""".r

  @tailrec
  def move(visited: Set[(Long, Long)],
           p: (Long, Long),
           d: (Int, Int),
           steps: List[String]): Long = steps match {
    case step :: tail =>
      val (newDir, dist) = step match {
        case R(n) => ((d._2 * -1, d._1 * 1), n.toLong)
        case L(n) => ((d._2 * 1, d._1 * -1), n.toLong)
      }

      val blocks =
        (1L to dist).map(d => (p._1 + newDir._1 * d, p._2 + newDir._2 * d))
      val beenThere = blocks.find(visited.contains)

      if (beenThere.isDefined) abs(beenThere.get._1) + abs(beenThere.get._2)
      else move(visited ++ blocks, blocks.last, newDir, tail)

    case Nil => throw new IllegalStateException("Not found.")
  }

  val input  = Source.fromFile("input/day1.txt").mkString.trim.split(",").toList
  val result = move(Set((0, 0)), (0, 0), (1, 0), input)
  println(result)
}
