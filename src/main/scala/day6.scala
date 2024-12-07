package io.feoktant

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{CommandLineParser, Using}

enum Direction:
  case Up, Right, Down, Left
  def next(x: Int, y: Int): (Int, Int) = this match
    case Up => (x - 1) -> y
    case Left => x -> (y - 1)
    case Down => (x + 1) -> y
    case Right => x -> (y + 1)

case class GuardPos(x: Int, y: Int, dir: Direction):
  private def rotate(t: (Int, Int)): (Int, Int) = (-t._2, t._1)
  def turnRight: GuardPos =
    val newOrd = Direction.fromOrdinal((dir.ordinal + 1) % Direction.values.length)
    copy(dir = newOrd)
  def nextPos: GuardPos =
    val (newX, newY) = dir.next(x, y)
    GuardPos(newX, newY, dir)
  def point: (Int, Int) = x -> y

case class Day6Input(
  map: Array[Array[Char]],
  guardPos: GuardPos,
)

given CommandLineParser.FromString[Day6Input] with
  def fromString(value: String): Day6Input =
    val map: Array[Array[Char]] = Using.resource(Source.fromResource(value)) {
      _.getLines().toList.map(_.toCharArray)}
      .toArray

    val pos = (for {
      x <- map.indices
      y <- map.indices
      if map(x)(y) == '^'
    } yield GuardPos(x, y, Direction.Up)).toList.head

    Day6Input(map, pos)

@main def day6(input: Day6Input): Unit =
  val Day6Input(map, guardPos) = input

  def getPath(start: GuardPos, obstacle: Option[(Int, Int)] = None): Option[Set[GuardPos]] =
    @tailrec
    def loop(pos: GuardPos, acc: Set[GuardPos]): Option[Set[GuardPos]] =
      val newPos = pos.nextPos
      if (!map.indices.contains(newPos.x) || !map(0).indices.contains(newPos.y))
        Some(acc)
      else if (map(newPos.x)(newPos.y) == '#' || obstacle.contains(newPos.point))
        loop(pos.turnRight, acc)
      else if (acc(newPos)) None
      else loop(newPos, acc + newPos)
    loop(start, Set(start))

  // answer1
  val visitedPoints = getPath(guardPos).getOrElse(Set.empty).map(_.point)
  println(visitedPoints.size)

  val pathWithObstacle = (visitedPoints - guardPos.point)
    .map(p => getPath(guardPos, Some(p)))
  println(pathWithObstacle.count(_.isEmpty))
