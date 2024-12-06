package io.feoktant

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{CommandLineParser, Using}
import scala.collection.mutable

case class Day5Input(rules: Map[Int, Set[Int]], pages: Seq[List[Int]])

given CommandLineParser.FromString[Day5Input] with
  private val rulesRegex = raw"(\d{2})\|(\d{2})".r
  private val pages = mutable.ArrayBuffer.empty[List[Int]]
  private val rulesTuples = mutable.ArrayBuffer.empty[(Int, Int)]
  def fromString(value: String): Day5Input =
    val (r, p) = Source.fromResource(value).getLines().span(_.isBlank)
    Using.resource(Source.fromResource(value)):
      _.getLines().foreach:
        case rulesRegex(r1, r2) => rulesTuples.addOne(r1.toInt -> r2.toInt)
        case str if str.isBlank => ()
        case str => pages.addOne(str.split(',').map(_.toInt).toList)
    val rules = rulesTuples.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    Day5Input(rules.withDefaultValue(Set.empty), pages.toSeq)

def takeMiddle[T](list: Seq[T]): Option[T] =
  list.drop(list.size / 2).headOption

@tailrec
def isSorted[T](list: List[T])(using ord: Ordering[T]): Boolean = list match
  case Nil | _ :: Nil => true
  case head :: tail => ord.compare(head, tail.head) != -1 && isSorted(tail)

@main def day5Alternative(input: Day5Input): Unit =
  val Day5Input(rulesMap, pages) = input

  given Ordering[Int] = (x: Int, y: Int) =>
    if (rulesMap(x)(y)) 1 else if (rulesMap(y)(x)) -1 else 0

  val answer1 = pages.filter(isSorted).flatMap(takeMiddle).sum
  println(answer1)

  val answer2 = pages.filterNot(isSorted).map(_.sorted).flatMap(takeMiddle).sum
  println(answer2)
