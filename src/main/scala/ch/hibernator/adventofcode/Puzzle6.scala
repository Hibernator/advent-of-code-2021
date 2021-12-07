package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Puzzle6 extends App:
  private val source = Source.fromFile("input6.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val initialFish = input.head.split(",").map(_.toInt).toSeq
  val initialMap: Map[Int, Long] = initialFish.foldLeft(Map[Int, Long]()) { case (map, fish) =>
    map.updated(fish, map.get(fish).map(_ + 1).getOrElse(1))
  }

  val finalMap = (1 to 256).foldLeft(initialMap) { case (previousMap, _) =>
    val timersDown = previousMap.map { case (timer, numFish) =>
      (timer - 1, numFish)
    }
    val numNewFish = timersDown.getOrElse(-1, 0L)
    val newFishAdded = if (numNewFish > 0) timersDown.updated(8, numNewFish) else timersDown
    val fishReset = newFishAdded.updated(6, newFishAdded.getOrElse(6, 0L) + numNewFish)
    fishReset.removed(-1)
  }

  val result = finalMap.values.sum

  println(result)

end Puzzle6
