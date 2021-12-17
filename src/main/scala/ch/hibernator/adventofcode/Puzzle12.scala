package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.{Set, SortedMap, immutable, mutable}
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Puzzle12 extends App:
  private val source = Source.fromFile("input/input12.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  // determines whether the String (cave) is big or small
  object StringExtensions {
    extension (str: String) {
      def isBig: Boolean = str == str.toUpperCase
      def isSmall: Boolean = str == str.toLowerCase
    }
  }

  import StringExtensions._

  // incidence map of the graph. Key is a node. Value is a set of all nodes adjacent to the key node.
  val incidenceMap: Map[String, Set[String]] = input.foldLeft(Map[String, Set[String]]()) { case (map, edge) =>
    val (start, end) = edge.split("-").pipe(startAndEnd => (startAndEnd.head, startAndEnd.last))
    val updatedStart = map.updated(start, map.getOrElse(start, Set[String]()).union(Set(end)))
    updatedStart.updated(end, updatedStart.getOrElse(end, Set[String]()).union(Set(start)))
  }

  // finds all paths from the existing path to the end visiting every small cave only once
  def findPaths(existingPath: Seq[String], graph: Map[String, Set[String]]): Seq[Seq[String]] =
    if existingPath.last == "end" then Seq(existingPath)
    else
      val possibleNextNodes = graph(existingPath.last)
        .filterNot(node => node.isSmall && existingPath.contains(node))
      if possibleNextNodes.isEmpty then Nil
      else
        possibleNextNodes.toSeq.flatMap { nextNode =>
          findPaths(existingPath :+ nextNode, graph)
        }

  val result1 = findPaths(Seq("start"), incidenceMap)

  println(result1.size)

  val startAndEnd = Set("start", "end")

  val allSmallNodes = input.foldLeft(Set[String]()) { case (accNodes, edge) =>
    val (start, end) = edge.split("-").pipe(startAndEnd => (startAndEnd.head, startAndEnd.last))
    val updatedStart = if start.isSmall && !startAndEnd.contains(start) then accNodes.union(Set(start)) else accNodes
    if end.isSmall && !startAndEnd.contains(end) then updatedStart.union(Set(end)) else updatedStart
  }

  // a collection of incidence maps (graphs) whose size is equal to the number of small nodes in the original graph
  // in each of the new graphs, one of the small nodes is duplicated
  // there are two nodes instead of the original one, each with the same connections as the original node
  val expandedIncidenceMaps: Set[Map[String, Set[String]]] = allSmallNodes.map { node =>
    val adjacentNodes = incidenceMap(node)
    val withDuplicatedNodes = incidenceMap
      .updated(s"${node}one", adjacentNodes)
      .updated(s"${node}two", adjacentNodes)
      .removed(node)

    withDuplicatedNodes.map { case (key, value) =>
      val newAdjacentNodes =
        if value.contains(node) then value.union(Set(s"${node}one", s"${node}two")).diff(Set(node))
        else value
      (key, newAdjacentNodes)
    }
  }

  val result2 = expandedIncidenceMaps
    .flatMap(findPaths(Seq("start"), _))
    .map(_.map(_.replace("one", "").replace("two", "")).mkString(","))

  println(result2.size)

end Puzzle12
