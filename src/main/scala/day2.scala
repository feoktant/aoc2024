package io.feoktant

import scala.io.Source
import scala.util.{CommandLineParser, Using}

given CommandLineParser.FromString[List[List[Int]]] with
  def fromString(value: String): List[List[Int]] =
    Using.resource(Source.fromResource(value))(_.getLines()
      .map(_.split(' ').map(_.toInt).toList)
      .toList)

enum Level:
  case INCREASE, DECREASE, UNSAFE

@main def day2(args: List[List[Int]]): Unit =

  def toLevelList(list: List[Int]): List[Level] =
    list.sliding(2).collect {
      case h1 :: h2 :: _ if h1 == h2 => Level.UNSAFE
      case h1 :: h2 :: _ if (h1 - h2).abs < 4 =>
        if (h1 > h2) Level.DECREASE else Level.INCREASE
      case h1 :: h2 :: _ => Level.UNSAFE
    }.toList // WTF?! if remove, will be wrong

  def isSafe(list: List[Level]): Boolean =
    list.forall(_ == Level.INCREASE) ||
      list.forall(_ == Level.DECREASE)

  val answer1 = args.map(toLevelList).count(isSafe)
  println(answer1)

  def permutations(l: List[Int]): List[List[Int]] =
    l.indices.map: i =>
        val (a, b) = l.splitAt(i)
        a ++ b.tail
      .toList

  val answer2 = args.map(l => (l :: permutations(l)).map(toLevelList))
    .count(_.exists(isSafe))
  println(answer2)
