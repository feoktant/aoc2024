package io.feoktant

import scala.io.Source
import scala.util.{CommandLineParser, Using}

case class Day12Input(
  theMap: Array[Array[Char]],
)

given CommandLineParser.FromString[Day12Input] with
  def fromString(value: String): Day12Input =
    val chars = Using.resource(Source.fromResource(value))(
      _.getLines().toList.map(_.toCharArray)).toArray
    Day12Input(chars)

@main def day12(input: Day12Input): Unit =
  // get array
  input.theMap.map(_.mkString).foreach(println)
  // get regions
  // count area and perimeter of each region
  // multiply a&p
  // sum
