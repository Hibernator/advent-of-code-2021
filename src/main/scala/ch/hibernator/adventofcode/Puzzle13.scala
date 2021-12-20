package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.{Set, SortedMap, immutable, mutable}
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Puzzle13 extends App:
  private val source = Source.fromFile("input/input131.txt")
  val inputDots: Seq[String] = source.getLines().toSeq
  source.close()

  private val source2 = Source.fromFile("input/input132.txt")
  val inputFolds: Seq[String] = source2.getLines().toSeq
  source2.close()

  case class Coordinates(x: Int, y: Int)
  case class Fold(axis: String, coordinate: Int)

  val originalDots: Set[Coordinates] = inputDots.foldLeft(Set[Coordinates]()) { case (dotsAcc, rawCoordinates) =>
    val coordinates = rawCoordinates.split(",").map(_.toInt)
    dotsAcc.union(Set(Coordinates(coordinates(0), coordinates(1))))
  }

  val folds: Seq[Fold] = inputFolds.foldLeft(Seq[Fold]()) { case (foldsAcc, rawFold) =>
    val fold = rawFold.split(" ")(2).split("=").pipe { fold =>
      Fold(fold(0), fold(1).toInt)
    }
    foldsAcc :+ fold
  }

  // folds the paper and returns the new set of dots coordinates
  def foldPaper(dots: Set[Coordinates], fold: Fold): Set[Coordinates] =
    fold.axis match {
      case "x" =>
        val (unfoldedDots, dotsToFold) = (dots.filter(_.x < fold.coordinate), dots.filter(_.x > fold.coordinate))
        dotsToFold.foldLeft(unfoldedDots) { case (dotsAcc, dot) =>
          val newDot = dot.copy(x = dot.x - 2 * (dot.x - fold.coordinate))
          dotsAcc.union(Set(newDot))
        }
      case "y" =>
        val (unfoldedDots, dotsToFold) = (dots.filter(_.y < fold.coordinate), dots.filter(_.y > fold.coordinate))
        dotsToFold.foldLeft(unfoldedDots) { case (dotsAcc, dot) =>
          val newDot = dot.copy(y = dot.y - 2 * (dot.y - fold.coordinate))
          dotsAcc.union(Set(newDot))
        }
    }

  val foldedOnce = foldPaper(originalDots, folds.head)
  val result1 = foldedOnce.size

  println(result1)

  val foldedCompletely = folds.foldLeft(originalDots) { case (currentDots, fold) =>
    foldPaper(currentDots, fold)
  }

  val maxX = foldedCompletely.map(_.x).max
  val maxY = foldedCompletely.map(_.y).max

  for {
    y <- 0 to maxY
    x <- 0 to maxX
  } {
    if foldedCompletely.contains(Coordinates(x, y)) then print("#") else print(" ")
    if x == maxX then println()
  }

end Puzzle13
