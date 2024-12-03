package io.feoktant

import scala.io.Source
import scala.util.{CommandLineParser, Using}

given CommandLineParser.FromString[List[String]] with
  def fromString(value: String): List[String] =
    Using.resource(Source.fromResource(value))(_.getLines().toList)

def toPairs(lines: List[String]): List[(Int, Int)] =
  val regex = raw"mul\((\d{1,3}),(\d{1,3})\)".r
  lines.flatMap: line =>
    regex.findAllMatchIn(line).map: m =>
      m.group(1).toInt -> m.group(2).toInt

def calculate(lines: List[String]): Int =
  toPairs(lines).map { case (t1, t2) => t1 * t2 }.sum

@main def day3(lines: List[String]): Unit =
  val filteredLines = lines.mkString
    .split(raw"do\(\)")
    .flatMap(_.split(raw"don't\(\)").headOption)
    .toList

  println(calculate(lines))
  println(calculate(filteredLines))
