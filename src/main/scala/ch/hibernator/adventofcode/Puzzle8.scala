package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.{SortedMap, Set, immutable, mutable}
import scala.io.Source

object Puzzle8 extends App:
  private val source = Source.fromFile("input/input8.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val allSegments: Set[Char] = Set('a', 'b', 'c', 'd', 'e', 'f', 'g')
  val allNumbers: Set[Int] = Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

  val numToSegments: Map[Int, Set[Char]] = Map(
    0 -> Set('a', 'b', 'c', 'e', 'f', 'g'),
    1 -> Set('c', 'f'),
    2 -> Set('a', 'c', 'd', 'e', 'g'),
    3 -> Set('a', 'c', 'd', 'f', 'g'),
    4 -> Set('b', 'c', 'd', 'f'),
    5 -> Set('a', 'b', 'd', 'f', 'g'),
    6 -> Set('a', 'b', 'd', 'e', 'f', 'g'),
    7 -> Set('a', 'c', 'f'),
    8 -> Set('a', 'b', 'c', 'd', 'e', 'f', 'g'),
    9 -> Set('a', 'b', 'c', 'd', 'f', 'g')
  )

  val numToSegmentsStr: Map[Int, String] = Map(
    0 -> "abcefg",
    1 -> "cf",
    2 -> "acdeg",
    3 -> "acdfg",
    4 -> "bcdf",
    5 -> "abdfg",
    6 -> "abdefg",
    7 -> "acf",
    8 -> "abcdefg",
    9 -> "abcdfg"
  )

  val segmentsToNum: Map[String, Int] = invertMap(numToSegmentsStr)

  // Just to start the algorithm, all possible options
  val segmentToWireInitial: mutable.Map[Char, Set[Char]] = mutable.Map(
    'a' -> Set('a', 'b', 'c', 'd', 'e', 'f', 'g'),
    'b' -> Set('a', 'b', 'c', 'd', 'e', 'f', 'g'),
    'c' -> Set('a', 'b', 'c', 'd', 'e', 'f', 'g'),
    'd' -> Set('a', 'b', 'c', 'd', 'e', 'f', 'g'),
    'e' -> Set('a', 'b', 'c', 'd', 'e', 'f', 'g'),
    'f' -> Set('a', 'b', 'c', 'd', 'e', 'f', 'g'),
    'g' -> Set('a', 'b', 'c', 'd', 'e', 'f', 'g')
  )

  def invertMap[T, U](map: Map[T, U]): Map[U, T] =
    map.map { case (key, value) =>
      (value, key)
    }

  // converts wires to a number given the wire to segment mapping
  def wiresToNumber(wires: String, wireToSegment: Map[Char, Char]): Option[Int] =
    val segments = wires.map(wireToSegment).sorted
    segmentsToNum.get(segments)

  case class Display(allNumbers: Seq[String], displayedSegments: Seq[String]):
    // returns the correct wire to segment mapping (can be used to decode numbers)
    def wireToSegment: Map[Char, Char] =
      val segmentToWire = mutable.Map.from(segmentToWireInitial)

      def updateKnownMappings(segments: Set[Char], possibleWires: Set[Char]): Unit =
        // limit the possible wires of the segments in question
        segments.foreach { segment =>
          val currentPossibleWires = segmentToWire(segment)
          segmentToWire.put(segment, possibleWires.intersect(currentPossibleWires))
        }

        // eliminate the possible wires for the segments from the other segments
        allSegments.diff(segments).foreach { otherSegment =>
          segmentToWire.put(otherSegment, segmentToWire(otherSegment).diff(possibleWires))
        }

      // update mappings and eliminate options for numbers 1, 4, 7 (unique number of segments)
      // the result is much reduced segmentToWireInitial map
      updateKnownMappings(numToSegments(1), allNumbers.find(_.length == 2).get.toCharArray.toSet)
      updateKnownMappings(numToSegments(7), allNumbers.find(_.length == 3).get.toCharArray.toSet)
      updateKnownMappings(numToSegments(4), allNumbers.find(_.length == 4).get.toCharArray.toSet)

      @tailrec
      def createMappings(
          remainingSegments: Set[Char],
          partialMappings: Seq[Map[Char, Char]]
      ): Seq[Map[Char, Char]] =
        if (remainingSegments.isEmpty) partialMappings
        else
          val currentSegment = remainingSegments.head
          val possibleWiresForSegment = segmentToWire(currentSegment)
          val newPartialMappings = partialMappings.flatMap { partialMapping =>
            possibleWiresForSegment.filterNot(partialMapping.values.toSeq.contains).toSeq.map { possibleWire =>
              partialMapping.updated(currentSegment, possibleWire)
            }
          }
          createMappings(remainingSegments.tail, newPartialMappings)

      // create all possible wire to segment mappings with the remaining options
      val firstCurrentSegment = allSegments.head
      val possibleWiresForFirstSegment = segmentToWire(firstCurrentSegment)
      val firstPartialMappings =
        possibleWiresForFirstSegment.toSeq.map { possibleWire =>
          Map(firstCurrentSegment -> possibleWire)
        }
      val allPossibleSegmentToWireMappings = createMappings(allSegments.tail, firstPartialMappings).distinct
      val allPossibleWireToSegmentMappings = allPossibleSegmentToWireMappings.map(invertMap)

      // checks whether the mapping works (every number mapped once)
      def isWireToSegmentMappingGood(mapping: Map[Char, Char]): Boolean =
        allNumbers.flatMap(wiresToNumber(_, mapping)).size == 10

      // finds the only correct mapping from all the remaining mappings
      val correctMapping = allPossibleWireToSegmentMappings
        .find(isWireToSegmentMappingGood)
        .getOrElse(sys.error("couldn't find correct mapping"))
      correctMapping

    // returns the displayed numbers
    def displayedNumbers: Seq[Int] =
      val correctMapping = wireToSegment
      displayedSegments.flatMap(wiresToNumber(_, correctMapping))

  end Display

  val displays = input
    .map { line =>
      line.split(" \\| ").take(2).map(_.split(" ").map(_.sorted))
    }
    .map { line =>
      Display(line.head.toSeq, line.last.toSeq)
    }

  val result1 = displays.map { display =>
    display.displayedSegments.count { number =>
      number.length == 2 || number.length == 4 || number.length == 3 || number.length == 7
    }
  }.sum

  println(result1)

  val result2 = displays
    .map(_.displayedNumbers)
    .map { numbers =>
      numbers.head * 1000 + numbers(1) * 100 + numbers(2) * 10 + numbers(3)
    }
    .sum
  println(result2)

end Puzzle8
