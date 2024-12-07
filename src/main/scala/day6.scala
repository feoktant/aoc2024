package io.feoktant

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{CommandLineParser, Using}

enum Direction:
  case Up, Right, Down, Left
  def next(point: (Int, Int)): (Int, Int) =
    val (x, y) = point
    this match
      case Up => (x - 1) -> y
      case Left => x -> (y - 1)
      case Down => (x + 1) -> y
      case Right => x -> (y + 1)

case class GuardPos(point: (Int, Int), dir: Direction):
  private def rotate(t: (Int, Int)): (Int, Int) = (-t._2, t._1)
  def turnRight: GuardPos =
    val newOrd = Direction.fromOrdinal((dir.ordinal + 1) % Direction.values.length)
    copy(dir = newOrd)
  def nextPos: GuardPos = GuardPos(dir.next(x, y), dir)
  def x: Int = point._1
  def y: Int = point._2

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
      y <- map(x).indices
      if map(x)(y) == '^'
    } yield GuardPos((x, y), Direction.Up)).toList.head

    Day6Input(map, pos)

@main def day6(input: Day6Input): Unit =
  val Day6Input(map, guardPos) = input
  val indices = map.indices.flatMap(x => map(x).indices.map(x -> _)).toSet

  def getPath(start: GuardPos, obstacle: Option[(Int, Int)] = None): Option[Set[GuardPos]] =
    @tailrec
    def loop(pos: GuardPos, acc: Set[GuardPos]): Option[Set[GuardPos]] =
      val newPos = pos.nextPos
      if (!indices.contains(newPos.point)) Some(acc)
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
