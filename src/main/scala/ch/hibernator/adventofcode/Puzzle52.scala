package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Puzzle52 extends App:
  private val source = Source.fromFile("input/input5.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  case class Coordinates(x: Int, y: Int)
  case class Tile(coordinates: Coordinates, vents: Int = 0)
  case class VentLine(start: Coordinates, end: Coordinates) {
    def isStraight: Boolean = start.x == end.x || start.y == end.y || ((start.x - end.x).abs == (start.y - end.y).abs)

    def normalize: VentLine =
      if (start.x == end.x) then if (start.y > end.y) then VentLine(end, start) else this
      else if (start.y == end.y) then if (start.x > end.x) then VentLine(end, start) else this
      else if (start.x > end.x) then VentLine(end, start)
      else this

    def isDiagonal: Boolean = start.x != end.x && start.y != end.y

    def lineCoordinates: Seq[Coordinates] =
      if (isDiagonal) then
        if (start.y > end.y) then
          val xs = (start.x to end.x).map(identity)
          val ys = (end.y to start.y).map(identity).reverse
          xs.zip(ys).map { case (x, y) => Coordinates(x, y) }
        else
          val xs = (start.x to end.x).map(identity)
          val ys = (start.y to end.y).map(identity)
          xs.zip(ys).map { case (x, y) => Coordinates(x, y) }
      else if (start.x == end.x) then (start.y to end.y).map(x => Coordinates(start.x, x))
      else (start.x to end.x).map(y => Coordinates(y, start.y))
  }

  val ventLines = input
    .map { in =>
      in.split(" -> ")
        .map(_.split(",").map(_.toInt))
        .map(rawCoordinates => Coordinates(rawCoordinates.head, rawCoordinates.last))
    }
    .map(startAndEndCoordinates => VentLine(startAndEndCoordinates.head, startAndEndCoordinates.last))
    .filter(_.isStraight)
    .map(_.normalize)

  val ventMap: mutable.Map[Coordinates, Int] = mutable.Map()

  val linesCoordinates = ventLines.map(_.lineCoordinates)

  ventLines.foreach { ventLine =>
    ventLine.lineCoordinates.foreach { vent =>
      ventMap.get(vent).fold(ventMap.put(vent, 1)) { knownVents =>
        ventMap.put(vent, knownVents + 1)
      }
    }
  }

  val result1 = ventMap.count { case (_, vents) =>
    vents > 1
  }

  println(result1)

end Puzzle52
