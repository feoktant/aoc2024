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

def toLevelList(list: List[Int]): List[Level] =
  list.sliding(2).collect:
    case h1 :: h2 :: _ if h1 == h2 => Level.UNSAFE
    case h1 :: h2 :: _ if (h1 - h2).abs < 4 =>
      if (h1 > h2) Level.DECREASE else Level.INCREASE
    case h1 :: h2 :: _ => Level.UNSAFE
  .toList // WTF?! if remove, will be wrong

def isSafe(list: List[Level]): Boolean =
  list.forall(_ == Level.INCREASE) || list.forall(_ == Level.DECREASE)

def permutations(l: List[Int]): List[List[Int]] =
  l.indices.map: i =>
    val (a, b) = l.splitAt(i)
    a ++ b.tail
  .toList

@main def day2(args: List[List[Int]]): Unit =
  val answer1 = args.map(toLevelList).count(isSafe)
  println(answer1)

  val answer2 = args.map(l => (l :: permutations(l)).map(toLevelList))
    .count(_.exists(isSafe))
  println(answer2)

  println("Solution by ahaan1984, rewritten from Python to Scala")
  val altAnswer2 = args.count(checkSafeDampener)
  println(altAnswer2)

// ------ Inspired by https://x.com/ahaan1984/status/1863607699126296614

def checkSafe(level: List[Int]): Boolean =
  @scala.annotation.tailrec
  def loop(lvl: List[Int]): Boolean = lvl match
    case h1 :: h2 :: _ =>
      if (1 to 3).contains((h1 - h2).abs) then loop(lvl.tail) else false
    case _ => true

  Set(level.sorted, level.sorted.reverse).contains(level) && loop(level)

def checkSafeDampener(level: List[Int]): Boolean =
  level.indices.exists: idx =>
    checkSafe(level.take(idx) ++ level.drop(idx + 1))
