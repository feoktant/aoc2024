package io.feoktant

import scala.io.Source
import scala.util.{CommandLineParser, Using}

given CommandLineParser.FromString[List[ClawMachine]] with
  def fromString(value: String): List[ClawMachine] =
    val str = raw"Button A: X\+(\d{1,3}), Y\+(\d{1,3})Button B: X\+(\d{1,3}), Y\+(\d{1,3})Prize: X=(\d{3,5}), Y=(\d{3,5})".r
    Using.resource(Source.fromResource(value))(
      _.getLines().toList.grouped(4).toList.map(_.mkString).collect {
        case str(xA, yA, xB, yB, x, y) =>
          ClawMachine(
            xA.toLong -> yA.toLong,
            xB.toLong -> yB.toLong,
            x.toLong,
            y.toLong,
          )
      })

case class ClawMachine(
  a: (Long, Long),
  b: (Long, Long),
  x: Long,
  y: Long,
) {
  def solve: Option[(Long, Long)] =
    val (xA, yA) = a
    val (xB, yB) = b
    val A = (x * yB - y * xB) / (xA * yB - xB * yA)
    val B = (x * yA - y * xA) / (xB * yA - yB * xA)
    Some(A -> B)
      .filter((a, b) => ((a * xA + b * xB) == x) && ((a * yA + b * yB) == y))
      .filter((a, b) => a >= 0 && b >= 0)
}

def toTokens(a: Long, b: Long): Long = a*3 + b

@main def day13(input: List[ClawMachine]): Unit =
  val answer = input.flatMap(_.solve)
    .filter((a, b) => a <= 100 && b <= 100)
    .map(toTokens).sum
  println(answer)

  val answer2 = input.map(c => c.copy(x = c.x + 10000000000000L, y = c.y + 10000000000000L))
    .flatMap(_.solve).map(toTokens).sum
  println(answer2)
