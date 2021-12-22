package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.{Set, SortedMap, immutable, mutable}
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Puzzle14 extends App:
  private val source = Source.fromFile("input/input14.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  case class RuleInput(element1: Char, element2: Char)

  val startingPolymer: String = input.head

  val rules: Map[RuleInput, Char] = input.drop(2).foldLeft(Map[RuleInput, Char]()) { case (rulesAcc, rawRule) =>
    rulesAcc + (RuleInput(rawRule.head, rawRule(1)) -> rawRule.last)
  }

  // performs one step of the polymer expansion
  // this takes too long when the polymer grows too much, therefore the method is only suitable for part 1
  def performInsertion(polymer: String): String =

    @tailrec
    def insertRecursively(processed: String, unprocessed: String): String =
      if unprocessed.isEmpty then processed
      else
        val toInsert = rules(RuleInput(processed.last, unprocessed.head))
        insertRecursively(s"$processed$toInsert${unprocessed.head}", unprocessed.tail)

    insertRecursively(polymer.head.toString, polymer.tail)

  val expandedPolymer = (1 to 10).foldLeft(startingPolymer) { case (polymer, _) => performInsertion(polymer) }

  val quantities = expandedPolymer.groupBy(identity).map { case (key, value) => (key, value.length) }.toSeq.sortBy(_._2)
  val (mostCommon, leastCommon) = (quantities.last._2, quantities.head._2)
  val result1 = mostCommon - leastCommon
  println(result1)

  // part 2 starts here. A more efficient algorithm is needed
  // the idea is, that the polymer is represented as a map, where key is the element pair and value is the number
  // of occurences of such pair in the polymer
  // This should make the polymer expansion step more efficient, as well just be replacing values in the map
  // We won't know what the polymer looks like, but we'll know enough to find the most and least common element
  case class Pair(element1: Char, element2: Char)

  val emptyPolymer = rules.map { case (rule, _) => (Pair(rule.element1, rule.element2), 0L) }
  val initialPairsToQuantities: Map[Pair, Long] =
    startingPolymer.sliding(2).foldLeft(emptyPolymer) { case (mapAcc, pair) =>
      val newPair = Pair(pair.head, pair.last)
      mapAcc.updated(newPair, mapAcc.getOrElse(newPair, 0L) + 1)
    }

  def expandPolymer(polymer: Map[Pair, Long]): Map[Pair, Long] =
    rules.foldLeft(polymer) { case (newPolymer, (rule, newElement)) =>
      // check whether the pair defined by the rule exists in the original polymer
      val rulePair = Pair(rule.element1, rule.element2)
      val maybeExistingPairQuantity = polymer.get(rulePair)
      maybeExistingPairQuantity match {
        case None => newPolymer // if the pair doesn't exist, the rule has no effect
        case Some(quantity) => // otherwise update the map
          val (newPair1, newPair2) = (Pair(rule.element1, newElement), Pair(newElement, rule.element2))
          val removedPairs = newPolymer.updated(rulePair, newPolymer(rulePair) - quantity)
          val addedOnePair = removedPairs.updated(newPair1, removedPairs(newPair1) + quantity)
          addedOnePair.updated(newPair2, addedOnePair(newPair2) + quantity)
      }
    }

  val polymerExpanded = (1 to 40).foldLeft(initialPairsToQuantities) { case (polymerAcc, _) =>
    expandPolymer(polymerAcc)
  }

  val allElements = rules.keySet.flatMap(rule => Set(rule.element1, rule.element2))
  val lastElement = input.head.last

  // to find the amount of element, we check all element pairs that start with that element and sum their quantities
  // if the element was on the last position of the original polymer, one is added
  val elementsQuantities = allElements.foldLeft(Map[Char, Long]()) { case (mapAcc, element) =>
    val elementQuantity = polymerExpanded.foldLeft(0L) { case (quantityAcc, (pair, pairQuantity)) =>
      if pair.element1 == element then quantityAcc + pairQuantity
      else quantityAcc
    }
    val elementQuantityFinal = if element == lastElement then elementQuantity + 1 else elementQuantity
    mapAcc.updated(element, elementQuantityFinal)
  }

  val mostCommonElement = elementsQuantities.toSeq.maxBy(_._2)._2
  val leastCommonElement = elementsQuantities.toSeq.minBy(_._2)._2

  println(mostCommonElement - leastCommonElement)

end Puzzle14
