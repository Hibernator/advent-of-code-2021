package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Puzzle5 extends App:
  private val source = Source.fromFile("input/input5.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  case class Coordinates(x: Int, y: Int)
  case class Tile(coordinates: Coordinates, vents: Int = 0)
  case class VentLine(start: Coordinates, end: Coordinates) {
    def isStraight: Boolean = start.x == end.x || start.y == end.y

    def normalize: VentLine =
      if (start.x == end.x) then if (start.y > end.y) then VentLine(end, start) else this
      else if (start.x > end.x) then VentLine(end, start)
      else this

    def lineCoordinates: Seq[Coordinates] =
      if (start.x == end.x) then (start.y to end.y).map(x => Coordinates(start.x, x))
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

end Puzzle5
