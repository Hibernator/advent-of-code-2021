package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.{Set, SortedMap, immutable, mutable}
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Puzzle15 extends App:
  private val source = Source.fromFile("input/input15.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  case class Coordinates(x: Int, y: Int)

  val targetCoordinates = Coordinates(input.head.length - 1, input.size - 1)

  val graph: Map[Coordinates, Int] = input.zipWithIndex.foldLeft(Map[Coordinates, Int]()) {
    case (graphAcc, (xLine, y)) =>
      xLine.zipWithIndex.foldLeft(graphAcc) { case (graphLineAcc, (dangerRaw, x)) =>
        graphLineAcc.updated(Coordinates(x, y), dangerRaw.toString.toInt)
      }
  }

  case class Node(coordinates: Coordinates, danger: Int)

  case class Vertex(coordinates: Coordinates, distanceFromSource: Int, previous: Option[Vertex])

  implicit val vertexOrdering: Ordering[Vertex] = (x: Vertex, y: Vertex) =>
    x.distanceFromSource.compare(y.distanceFromSource) * (-1)

  def getNeighbors(coordinates: Coordinates, map: Map[Coordinates, Int]): Set[Node] =
    Set(
      map
        .get(coordinates.copy(x = coordinates.x - 1))
        .map(danger => Node(coordinates.copy(x = coordinates.x - 1), danger)),
      map
        .get(coordinates.copy(x = coordinates.x + 1))
        .map(danger => Node(coordinates.copy(x = coordinates.x + 1), danger)),
      map
        .get(coordinates.copy(y = coordinates.y - 1))
        .map(danger => Node(coordinates.copy(y = coordinates.y - 1), danger)),
      map
        .get(coordinates.copy(y = coordinates.y + 1))
        .map(danger => Node(coordinates.copy(y = coordinates.y + 1), danger))
    ).flatten

  def shortestPath(source: Coordinates, target: Coordinates, map: Map[Coordinates, Int]): Seq[Vertex] =
    val sourceVertex = Vertex(source, 0, None)
    val vertexQueue = mutable.PriorityQueue[Vertex]()
    val visitedVertices = mutable.Set[Coordinates]()

    val coordinatesToVertex: mutable.Map[Coordinates, Vertex] = mutable.Map()

    // initialize the queue
    vertexQueue.addOne(sourceVertex)

    var closestVertexFromQueue: Vertex = sourceVertex

    while (closestVertexFromQueue.coordinates != target) {
      closestVertexFromQueue = vertexQueue.dequeue()
      if !visitedVertices.contains(closestVertexFromQueue.coordinates) then
        visitedVertices.addOne(closestVertexFromQueue.coordinates)
        coordinatesToVertex.put(closestVertexFromQueue.coordinates, closestVertexFromQueue)
        val neighbors = getNeighbors(closestVertexFromQueue.coordinates, map)
          .filterNot(node => visitedVertices.contains(node.coordinates))
        for {
          neighbor <- neighbors
        } {
          val alternativeDistanceFromSourceToNeighbor = closestVertexFromQueue.distanceFromSource + neighbor.danger
          if alternativeDistanceFromSourceToNeighbor < coordinatesToVertex
              .getOrElse(neighbor.coordinates, Vertex(neighbor.coordinates, Integer.MAX_VALUE, None))
              .distanceFromSource
          then
            coordinatesToVertex.put(
              neighbor.coordinates,
              Vertex(neighbor.coordinates, alternativeDistanceFromSourceToNeighbor, Some(closestVertexFromQueue))
            )
            vertexQueue.enqueue(
              Vertex(neighbor.coordinates, alternativeDistanceFromSourceToNeighbor, Some(closestVertexFromQueue))
            )
        }
    }

    val path: Seq[Vertex] =
      @tailrec
      def createPath(targetVertex: Vertex, acc: Seq[Vertex]): Seq[Vertex] =
        targetVertex.previous match {
          case None           => acc :+ targetVertex
          case Some(previous) => createPath(previous, acc :+ targetVertex)
        }
      createPath(closestVertexFromQueue, Seq())

    path

  val path = shortestPath(Coordinates(0, 0), targetCoordinates, graph)
  val result1 = path.head.distanceFromSource
  println(result1)

  val tileXSize = input.head.length
  val tileYSize = input.size
  val targetCoordinates2 =
    Coordinates(targetCoordinates.x + tileXSize * 4, targetCoordinates.y + tileYSize * 4)
  val graph2 = mutable.Map.from(graph)
  for {
    x <- 0 to targetCoordinates2.x
    y <- 0 to targetCoordinates2.y
  } {
    val newDanger = graph(Coordinates(x % tileXSize, y % tileYSize)) + x / tileXSize + y / tileYSize
    graph2.put(Coordinates(x, y), if newDanger > 9 then newDanger - 9 else newDanger)
  }
  val graph2Map: Map[Coordinates, Int] = graph2.toMap

  val path2 = shortestPath(Coordinates(0, 0), targetCoordinates2, graph2Map)
  val result2 = path2.head.distanceFromSource
  println(result2)

end Puzzle15
