package ch.hibernator.adventofcode

import scala.io.Source

object Puzzle1 extends App {
  private val source = Source.fromFile("input/input1.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val depths = input.map(_.toInt)

  val increases = depths.init
    .zip(depths.tail)
    .map { case (first, second) =>
      second > first
    }
    .count(_ == true)

  println(increases)

  val windowSums = depths.sliding(3).map(_.sum).toSeq
  val windowIncreases = windowSums.init
    .zip(windowSums.tail)
    .map { case (first, second) =>
      second > first
    }
    .count(_ == true)

  println(windowIncreases)
}
