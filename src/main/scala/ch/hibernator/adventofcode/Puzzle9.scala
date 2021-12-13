package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.{Set, SortedMap, immutable, mutable}
import scala.io.Source

object Puzzle9 extends App:
  private val source = Source.fromFile("input/input9.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  case class Coordinates(row: Int, column: Int)

  val floor: mutable.Map[Coordinates, Point] = mutable.Map()

  case class Point(coordinates: Coordinates, height: Int) {
    private def getAdjacentPoints: Set[Point] =
      Set(
        floor.get(Coordinates(coordinates.row - 1, coordinates.column)),
        floor.get(Coordinates(coordinates.row + 1, coordinates.column)),
        floor.get(Coordinates(coordinates.row, coordinates.column - 1)),
        floor.get(Coordinates(coordinates.row, coordinates.column + 1))
      ).flatten

    def isLow: Boolean = getAdjacentPoints.forall(height < _.height)

    def basinSize: Int =
      val basin: mutable.Set[Point] = mutable.Set()

      def findBasin(point: Point): Unit =
        if !basin.contains(point) then
          basin.add(point)
          val adjacentBasinPoints = point.getAdjacentPoints
            .filter(adjacentPoint => adjacentPoint.height != 9 && !basin.contains(adjacentPoint))
          adjacentBasinPoints.foreach(findBasin)

      findBasin(this)

      basin.size
  }

  input.zipWithIndex.foreach { case (line, row) =>
    line.toCharArray.map(_.toString.toInt).zipWithIndex.foreach { case (height, column) =>
      val coordinates = Coordinates(row, column)
      floor.put(coordinates, Point(coordinates, height))
    }
  }

  val lowPoints = floor.values.filter(_.isLow)
  val result1 = lowPoints.map(_.height + 1).sum
  println(result1)

  val basinSizes = lowPoints.map(_.basinSize).toSeq.sorted.reverse
  val result2 = basinSizes.take(3).product
  println(result2)

end Puzzle9
