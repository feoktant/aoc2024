package io.feoktant

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.{CommandLineParser, Using}

case class Day8Input(
  antennas: Map[Char, List[(Int, Int)]],
  x: Int,
  y: Int,
)

given CommandLineParser.FromString[Day8Input] with
  def fromString(value: String): Day8Input =
    val antennas = mutable.Map.empty[Char, mutable.ArrayBuffer[(Int, Int)]]
    var xx = 0
    var yy = 0
    Using.resource(Source.fromResource(value)): resource =>
      val it = resource.getLines().zipWithIndex
      while (it.hasNext) {
        val (line, x) = it.next()
        xx = xx.max(x)
        line.zipWithIndex.foreach: (ch, y) =>
          yy = yy.max(y)
          if (ch.isLetterOrDigit)
            antennas.updateWith(ch):
              case Some(v) => Some(v.addOne(x -> y))
              case None => Some(mutable.ArrayBuffer(x -> y))
          else ()
      }
    Day8Input(antennas.view.mapValues(_.toList).toMap, xx + 1, yy + 1)

@main def day8(in: Day8Input): Unit =
  val Day8Input(antennas, x, y) = in

  def getAntinodes(p1: (Int, Int), p2: (Int, Int)): List[(Int, Int)] =
    val ((x1, y1), (x2, y2)) = if (p1._1 < p2._1) (p1, p2) else (p2, p1)
    val xDiff = x1 - x2
    val yDiff = y1 - y2
    List(
      (x1 + xDiff) -> (y1 + yDiff),
      (x2 - xDiff) -> (y2 - yDiff),
    ).filter: (nX, nY) =>
      nX >= 0 && nX < x && nY >= 0 && nY < y

  @tailrec
  def permutations(
    ants: List[(Int, Int)],
    acc: List[((Int, Int), (Int, Int))] = List.empty,
  ): List[((Int, Int), (Int, Int))] =
    ants match
      case Nil => acc
      case h :: tt =>
        val tails = tt.map(t => h -> t)
        permutations(tt, tails ::: acc)

  val answer1 = antennas.view.values.flatMap { points =>
    permutations(points).flatMap((x,y) => getAntinodes(x, y))
  }.toSet.size

  println(answer1)
