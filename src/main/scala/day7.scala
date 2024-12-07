package io.feoktant

import scala.io.Source
import scala.math.pow
import scala.util.{CommandLineParser, Using}

case class Equation(
  testValue: Long,
  numbers: List[Long],
)

object Equation:
  def fromString(str: String): Equation =
    val (value, numbersString) = str.splitAt(str.indexOf(':'))
    val numbers = numbersString.drop(2).trim.split(' ').map(_.toLong).toList
    Equation(value.toLong, numbers)

given CommandLineParser.FromString[List[Equation]] with
  def fromString(value: String): List[Equation] =
    Using.resource(Source.fromResource(value)) {
      _.getLines().toList.map(Equation.fromString)
    }

def concat(l1: Long, l2: Long): Long =
  val strLength = l2.toString.length
  val mul = pow(10, strLength).toLong
  l1 * mul + l2

def isValid(e: Equation, checkList: (Long, Long) => List[Long]): Boolean =
  def loop(acc: List[List[Long]]): Boolean =
    acc.exists:
      case h1 :: h2 :: Nil => checkList(h1, h2).contains(e.testValue)
      case h1 :: h2 :: tail =>
        val newAcc = checkList(h1, h2).map(_ :: tail)
          .filter(_.head <= e.testValue)
        loop(newAcc)
      case _ => false
  loop(List(e.numbers))


@main def day7(input: List[Equation]): Unit =
  // Part 1
  val twoOp = (l1: Long, l2: Long) => List(l1 * l2, l1 + l2)
  println(input.filter(e => isValid(e, twoOp)).map(_.testValue).sum)
  // Part 2
  val threeOp = (l1: Long, l2: Long) => List(l1 * l2, l1 + l2, concat(l1, l2))
  println(input.filter(e => isValid(e, threeOp)).map(_.testValue).sum)
