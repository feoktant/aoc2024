package io.feoktant

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

def parse5(value: String): (Seq[(Int, Int)], Seq[List[Int]]) =
  val rulesRegex = raw"(\d{2})\|(\d{2})".r
  Using.resource(Source.fromResource(value))(
    _.getLines().foldLeft(Seq.empty[(Int, Int)] -> Seq.empty[List[Int]]):
     case ((rules, pages), rulesRegex(r1, r2)) =>
       ((r1.toInt, r2.toInt) +: rules, pages)
     case ((rules, pages), str) if !str.isBlank =>
       (rules, str.split(',').toList.map(_.toInt) +: pages)
     case (acc, str) => acc
 )

@main def day5(args: String*): Unit =
  val (rulesTuples, pages) = parse5(args(0))
  val rulesBefore = rulesTuples.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
  val rulesAfter = rulesTuples.groupBy(_._2).view.mapValues(_.map(_._1).toSet).toMap

  @tailrec
  def correctlyOrdered(next: List[Int], prev: List[Int] = Nil): Boolean = next match
    case Nil => true
    case head :: tail =>
      val maybeBeforeRule = rulesBefore.get(head)
      val maybeAfterRule = rulesAfter.get(head)
      tail.forall(t1 => maybeBeforeRule.forall(_.contains(t1)) &&
        maybeAfterRule.forall(!_.contains(t1)))
        && correctlyOrdered(tail, head :: prev)

  extension[T] (list: List[T])
    def middleElement: T = list.apply((list.size - 1) / 2)

  val answer1 = pages.filter(correctlyOrdered(_)).map(_.middleElement).sum
  println(answer1)

  // -------------
  val sort = (x: Int, y: Int) => rulesBefore.get(x).exists(_.contains(y))
  val answer2 = pages.filterNot(correctlyOrdered(_))
    .map(_.sortWith(sort))
    .map(_.middleElement)
    .sum
  println(answer2)
