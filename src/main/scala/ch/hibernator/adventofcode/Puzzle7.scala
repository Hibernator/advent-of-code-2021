package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Puzzle7 extends App:
  private val source = Source.fromFile("input/input7.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val initialPositions = input.head.split(",").map(_.toInt).toSeq
  val destinationToLinearConsumption: mutable.Map[Int, Int] = mutable.Map()

  (initialPositions.min to initialPositions.max).foreach { destination =>
    val consumption = initialPositions.map { position =>
      (position - destination).abs
    }.sum
    destinationToLinearConsumption.put(destination, consumption)
  }

  val result1 = destinationToLinearConsumption.values.min
  println(result1)

  val destinationToIncreasingConsumption: mutable.Map[Int, Int] = mutable.Map()

  (initialPositions.min to initialPositions.max).foreach { destination =>
    val increasingConsumption = initialPositions.map { position =>
      val distance = (position - destination).abs
      (1 to distance).sum
    }.sum
    destinationToIncreasingConsumption.put(destination, increasingConsumption)
  }

  val result2 = destinationToIncreasingConsumption.values.min
  println(result2)

end Puzzle7
