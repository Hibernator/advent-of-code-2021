package ch.hibernator.adventofcode

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Puzzle2 extends App {
  private val source = Source.fromFile("input/input2.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  enum Direction(val text: String):
    case Forward extends Direction("forward")
    case Down extends Direction("down")
    case Up extends Direction("up")

  object Direction:
    def of(textValue: String): Direction =
      Direction.values.find(_.text == textValue).get

  case class Movement(direction: Direction, distance: Int)

  val movements = input.map { move =>
    val (directionRaw, distanceRaw) =
      move.split(" ").pipe(array => (array.head, array.last))
    Movement(Direction.of(directionRaw), distanceRaw.toInt)
  }

  case class Coordinates(horizontal: Int, vertical: Int)

  val finalPosition = movements.foldLeft(Coordinates(0, 0)) { case (position, move) =>
    move.direction match {
      case Direction.Forward =>
        position.copy(horizontal = position.horizontal + move.distance)
      case Direction.Down =>
        position.copy(vertical = position.vertical + move.distance)
      case Direction.Up =>
        position.copy(vertical = position.vertical - move.distance)
    }
  }

  val result1 = finalPosition.vertical * finalPosition.horizontal
  println(result1)

  case class CoordinatesWithAim(horizontal: Int, vertical: Int, aim: Int)

  val finalPositionWithAim = movements.foldLeft(CoordinatesWithAim(0, 0, 0)) { case (position, move) =>
    move.direction match {
      case Direction.Forward =>
        position.copy(
          horizontal = position.horizontal + move.distance,
          vertical = position.vertical + position.aim * move.distance
        )
      case Direction.Down =>
        position.copy(aim = position.aim + move.distance)
      case Direction.Up =>
        position.copy(aim = position.aim - move.distance)
    }
  }

  val result2 = finalPositionWithAim.vertical * finalPositionWithAim.horizontal
  println(result2)
}
