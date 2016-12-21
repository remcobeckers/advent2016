import scala.io._
import scala.math._
import scala.annotation.tailrec

object Day5 extends App {
  val input = "ojvtpuvg"

  import java.security._

  private val md = MessageDigest.getInstance("MD5");

  def md5(str: String): Array[Byte] = md.digest(str.getBytes());

  def arrayToHex(bytes: Array[Byte]) =
    bytes.map(b => "%02x".format(b)).mkString

  def md5Hex = (arrayToHex _) compose md5

  // val result1 = Iterator
  //   .from(1)
  //   .map(n => md5(s"${ input }$n"))
  //   .collect {
  //     case arr if arr(0) == 0 && arr(1) == 0 && arr(2) >= 0 && arr(2) < 16 =>
  //       arrayToHex(arr)
  //   }
  //   .take(8)
  //   .map { v =>
  //     println(v); v
  //   }
  //   .map(_ (5))
  //   .mkString
  //
  // println(result1)

  val result2 = Iterator
    .from(1)
    .map(n => md5(s"${ input }$n"))
    .collect {
      case arr if arr(0) == 0 && arr(1) == 0 && arr(2) >= 0 && arr(2) < 16 =>
        arrayToHex(arr)
    }
    .scanLeft(Array[Char]('z', 'z', 'z', 'z', 'z', 'z', 'z', 'z')) {
      (acc, hash) =>
        val pos = hash(5).toInt - 48
        if (pos < 8 && acc(pos) == 'z') acc(pos) = hash(6)
        acc
    }
    .dropWhile(res => res.exists(_ == 'z'))
    .next
    .mkString

  println(result2)
}
