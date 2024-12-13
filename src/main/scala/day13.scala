package io.feoktant

import scala.io.Source
import scala.util.{CommandLineParser, Using}

//import scala.annotation.tailrec
//
//@tailrec
//def gcd(a: Int, b: Int): Int =
//  if (b == 0) a else gcd(b, a % b)
//
//def lcm(a: Int, b: Int): Int =
//  (a * b) / gcd(a, b)

given CommandLineParser.FromString[List[ClawMachine]] with
  def fromString(value: String): List[ClawMachine] =
    val str = raw"Button A: X\+(\d{1,3}), Y\+(\d{1,3})Button B: X\+(\d{1,3}), Y\+(\d{1,3})Prize: X=(\d{3,5}), Y=(\d{3,5})".r
    Using.resource(Source.fromResource(value))(
      _.getLines().toList.grouped(4).toList.map(_.mkString).collect {
        case str(xA, yA, xB, yB, x, y) =>
          ClawMachine(
            xA.toInt -> yA.toInt,
            xB.toInt -> yB.toInt,
            x.toInt,
            y.toInt,
          )
      })

case class ClawMachine(
  a: (Int, Int),
  b: (Int, Int),
  x: Int,
  y: Int,
) {
  def solve: Option[(Int, Int)] =
    val (xA, yA) = a
    val (xB, yB) = b
    val A = (x * yB - y * xB) / (xA * yB - xB * yA)
    val B = (x * yA - y * xA) / (xB * yA - yB * xA)
    Some(A -> B)
      .filter((a, b) => ((a * xA + b * xB) == x) && ((a * yA + b * yB) == y))
      .filter((a, b) => a <= 100 && b <= 100)
      .filter((a, b) => a >= 0 && b >= 0)
}


def toTokens(a: Int, b: Int): Int = a*3 + b

@main def day13(input: List[ClawMachine]): Unit =
//  val answer1 = ClawMachine(94 -> 34, 22 -> 67, 8400, 5400).solve.map(toTokens)
//  val answer2 = ClawMachine(26 -> 66, 67 -> 21, 12748, 12176).solve.map(toTokens)
//  val answer3 = ClawMachine(17 -> 86, 84 -> 37, 7870, 6450).solve.map(toTokens)
  val answer = input.flatMap(_.solve).map(toTokens).sum
  println(answer)