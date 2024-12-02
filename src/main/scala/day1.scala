package io.feoktant

import scala.io.Source
import scala.util.{CommandLineParser, Using}

given CommandLineParser.FromString[(List[Int], List[Int])] with
  def fromString(value: String): (List[Int], List[Int]) = {
    val input = raw"(\d+) {3}(\d+)".r
    Using.resource(Source.fromResource(value)) { _.getLines()
      .foldLeft(List.empty[Int] -> List.empty[Int]) { case ((listA, listB), line) =>
        line match {
          case input(a, b) => (a.toInt :: listA, b.toInt :: listB)
        }
      }
    }
  }

@main def day1(args: (List[Int], List[Int])): Unit = {
  val (listA, listB) = args

  val answer1 = listA.sorted.zip(listB.sorted)
    .foldLeft(0) { case (acc, (a, b)) => acc + (a - b).abs }
  println(answer1)

  val valueToCount = listB.groupBy(identity).view.mapValues(_.size)
  val answer2 = listA.foldLeft(0) { case (acc, a) =>
    acc + a * valueToCount.getOrElse(a, 0)
  }
  println(answer2)
}
