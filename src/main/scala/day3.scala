package io.feoktant

import scala.io.Source
import scala.util.Using

def parse(value: String): List[String] =
  Using.resource(Source.fromResource(value))(_.getLines().toList)

def toPairs(lines: List[String]): List[(Int, Int)] =
  val regex = raw"mul\((\d{1,3}),(\d{1,3})\)".r
  lines.flatMap: line =>
    regex.findAllMatchIn(line).map: m =>
      m.group(1).toInt -> m.group(2).toInt

def calculate(lines: List[String]): Int =
  toPairs(lines).map { case (t1, t2) => t1 * t2 }.sum

@main def day3(args: String*): Unit =
  val lines = parse(args(0))
  val filteredLines = lines.mkString
    .split(raw"do\(\)")
    .flatMap(_.split(raw"don't\(\)").headOption)
    .toList

  println(calculate(lines))
  println(calculate(filteredLines))

//  ------ Inspired by https://x.com/SammedC94865641/status/1863874091205484832
  val (sum, _) = raw"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)".r
    .findAllMatchIn(lines.mkString).foldLeft(0 -> true):
      case ((acc, isEnabled), mat) =>
        mat.group(0) match
          case "do()" => acc -> true
          case "don't()" => acc -> false
          case _ if !isEnabled => acc -> false
          case _ if isEnabled =>
            val n1 = mat.group(1).toInt
            val n2 = mat.group(2).toInt
            (n1 * n2 + acc) -> isEnabled
  println(sum)