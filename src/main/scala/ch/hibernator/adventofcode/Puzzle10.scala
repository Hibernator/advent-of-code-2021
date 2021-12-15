package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.{Set, SortedMap, immutable, mutable}
import scala.io.Source

object Puzzle10 extends App:
  private val source = Source.fromFile("input/input10.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  object CharExtensions {
    private val openingChars: Set[Char] = Set('(', '[', '{', '<')
    private val closingChars: Set[Char] = Set(')', ']', '}', '>')

    extension (ch: Char) {
      def opposite: Char =
        ch match {
          case ')' => '('
          case ']' => '['
          case '}' => '{'
          case '>' => '<'
          case '(' => ')'
          case '[' => ']'
          case '{' => '}'
          case '<' => '>'
        }

      def isOpening: Boolean = openingChars.contains(ch)
      def isClosing: Boolean = closingChars.contains(ch)
    }
  }

  val charToPoints: Map[Char, Int] = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)

  import CharExtensions._

  def findWrongCharacter(chunk: String): Option[Char] =

    @tailrec
    def processNext(alreadyProcessed: String, toProcess: String): Option[Char] =
      if toProcess.isEmpty then None
      else if toProcess.head.isOpening then processNext(s"$alreadyProcessed${toProcess.head}", toProcess.tail)
      else if alreadyProcessed.last.opposite == toProcess.head then processNext(alreadyProcessed.init, toProcess.tail)
      else Some(toProcess.head)

    processNext("", chunk)

  val result1 = input.flatMap(findWrongCharacter).map(charToPoints(_)).sum
  println(result1)

  val incompleteLines = input.filter(findWrongCharacter(_).isEmpty)

  def completeLine(line: String): String =

    def removeClosedBlocks(line: String): String =

      @tailrec
      def processNext(alreadyProcessed: String, toProcess: String): String =
        if toProcess.isEmpty then alreadyProcessed
        else if toProcess.head.isOpening then processNext(s"$alreadyProcessed${toProcess.head}", toProcess.tail)
        else if alreadyProcessed.last.opposite == toProcess.head then processNext(alreadyProcessed.init, toProcess.tail)
        else sys.error("Processing failed")

      processNext("", line)

    removeClosedBlocks(line).reverse.map(_.opposite)

  val completionChunks = incompleteLines.map(completeLine)

  val charToPoints2: Map[Char, Int] = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  def calculateChunkScore(chunk: String): Long =
    val points = chunk.map(charToPoints2(_))
    points.foldLeft(0L) { case (acc, nextPoint) =>
      acc * 5 + nextPoint
    }

  val chunkScores = completionChunks.map(calculateChunkScore)
  val result2 = chunkScores.sorted.apply(chunkScores.size / 2)

  println(result2)

end Puzzle10
