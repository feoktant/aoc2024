package io.feoktant

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{CommandLineParser, Using}
import Direction._

enum Direction:
  case Up, Right, Down, Left
  def nextDirection: Direction =
    val index = if (ordinal == Direction.values.length - 1) 0 else ordinal + 1
    Direction.fromOrdinal(index)

case class GuardPos(point: (Int, Int), dir: Direction):
  val (x, y) = point
  def turnRight: GuardPos = copy(dir = dir.nextDirection)
  def nextPos: GuardPos =
    val nextPoint = dir match
      case Up => (x - 1) -> y
      case Left => x -> (y - 1)
      case Down => (x + 1) -> y
      case Right => x -> (y + 1)
    GuardPos(nextPoint, dir)

case class Day6Input(
  map: Array[Array[Char]],
  guardPos: GuardPos,
)

given CommandLineParser.FromString[Day6Input] with
  def fromString(value: String): Day6Input =
    val theMap = Using.resource(Source.fromResource(value)) {
        _.getLines().toList.map(_.toCharArray) }
      .toArray
    val startPoint = theMap.zipWithIndex.find((arr, x) => arr.contains('^'))
      .map((arr, x) => x -> arr.indexOf('^'))
      .getOrElse(throw new IllegalArgumentException("Invalid situation map"))
    Day6Input(theMap, GuardPos(startPoint, Up))

@main def day6(input: Day6Input): Unit =
  val Day6Input(theMap, guardPos) = input
  val indices = theMap.indices.flatMap(x => theMap(x).indices.map(x -> _)).toSet
  val hashPoints = (theMap.zipWithIndex.collect: // this doesn't work
    case (arr, x) if arr.contains('#') =>  x -> arr.indexOf('#')).toSet

  def getPath(start: GuardPos, obstacle: Option[(Int, Int)] = None): Option[List[GuardPos]] =
    @tailrec
    def loop(pos: GuardPos, acc: Set[GuardPos]): Option[List[GuardPos]] =
      val newPos = pos.nextPos
      if (!indices.contains(newPos.point)) Some(acc.toList)
      else if (theMap(newPos.x)(newPos.y) == '#' || obstacle.contains(newPos.point))
        loop(pos.turnRight, acc)
      else if (acc(newPos)) None
      else loop(newPos, acc + newPos)
    loop(start, Set(start))

  // answer1
  val visitedPoints = getPath(guardPos).getOrElse(List.empty).map(_.point).distinct
  println(visitedPoints.size)

  // answer2
  val endlessLoopCount = visitedPoints.filterNot(_ == guardPos.point)
    .count(p => getPath(guardPos, Some(p)).isEmpty)
  println(endlessLoopCount)
