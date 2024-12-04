package io.feoktant

import scala.io.Source
import scala.util.Using

def parse(value: String): Array[Array[Char]] =
  Using.resource(Source.fromResource(value))(_.getLines().map(_.toCharArray).toArray)

type Word4 = ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
type XWord = ((Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int))

@main def day4(args: String*): Unit =
  val input = parse(args(0))

  def words(row: Int, col: Int): Seq[Word4] = {
    Seq(
      (row -> col, row -> (col + 1), row -> (col + 2), row -> (col + 3)),
      (row -> col, row -> (col - 1), row -> (col - 2), row -> (col - 3)),
      (row -> col, (row + 1) -> col, (row + 2) -> col, (row + 3) -> col),
      (row -> col, (row - 1) -> col, (row - 2) -> col, (row - 3) -> col),
      (row -> col, (row + 1) -> (col + 1), (row + 2) -> (col + 2), (row + 3) -> (col + 3)),
      (row -> col, (row - 1) -> (col - 1), (row - 2) -> (col - 2), (row - 3) -> (col - 3)),
      (row -> col, (row + 1) -> (col - 1), (row + 2) -> (col - 2), (row + 3) -> (col - 3)),
      (row -> col, (row - 1) -> (col + 1), (row - 2) -> (col + 2), (row - 3) -> (col + 3)),
    ).filter { case ((r1, c1), (r2, c2), (r3, c3), (r4, c4)) =>
      Seq(r1, r2, r3, r4).forall(r => input.indices.contains(r)) &&
        Seq(c1, c2, c3, c4).forall(c => input.head.indices.contains(c))
    }
  }

  def isXmas(word: Word4): Boolean = word match
    case ((r1, c1), (r2, c2), (r3, c3), (r4, c4)) =>
      input(r1)(c1) == 'X' &&
        input(r2)(c2) == 'M' &&
        input(r3)(c3) == 'A' &&
        input(r4)(c4) == 'S'

  val l = for {
    r <- input.indices
    c <- input.head.indices
    w <- words(r, c)
    if isXmas(w)
  } yield 1
  println(l.sum)

  // -------------------------------------------
  def words2(row: Int, col: Int): Option[XWord] = Some(
    ((row + 1) -> (col + 1), (row - 1) -> (col + 1), row -> col, (row - 1) -> (col - 1), (row + 1) -> (col - 1)),
    ).filter { case ((r1, c1), (r2, c2), (r3, c3), (r4, c4), (r5, c5)) =>
    Seq(r1, r2, r3, r4, r5).forall(r => input.indices.contains(r)) &&
      Seq(c1, c2, c3, c4, c5).forall(c => input.head.indices.contains(c))
  }

  def isXmas2(word: XWord): Boolean = word match
    case ((r1, c1), (r2, c2), (r3, c3), (r4, c4), (r5, c5)) =>
      input(r3)(c3) == 'A' && {
        (input(r1)(c1) == 'M' && input(r4)(c4) == 'S') ||
          (input(r1)(c1) == 'S' && input(r4)(c4) == 'M')
      } && {
        (input(r2)(c2) == 'M' && input(r5)(c5) == 'S') ||
          (input(r2)(c2) == 'S' && input(r5)(c5) == 'M')
      }

  val l2 = for {
    r <- input.indices
    c <- input.head.indices
    w <- words2(r, c)
    if isXmas2(w)
  } yield 1
  println(l2.sum)

  // -----
  val XMAS = "XMAS"
  val altAnswer1 = for
    i <- input.indices
    j <- input(i).indices
    (mi, mj) <- for
      mi <- -1 to 1
      mj <- -1 to 1
    yield (mi, mj)
    if (mi, mj) != (0, 0)
    if XMAS.zipWithIndex.forall: (char, k) =>
      val idx = i + mi * k
      val jdx = j + mj * k
      input.indices.contains(idx)
        && input(i).indices.contains(jdx)
        && input(idx)(jdx) == char
  yield ()
  println(altAnswer1.length)

  val altAnswer2 = for
    i <- 1 until input.length - 1
    j <- 1 until input(i).length - 1
    if input(i)(j) == 'A'
    chars = Seq((-1, -1), (-1, 1), (1, 1), (1, -1)).map: (x, y) =>
      input(i + x)(j + y)
    if Set("MSSM", "MMSS", "SSMM", "SMMS").contains(chars.mkString)
  yield ()
  println(altAnswer2.length)
