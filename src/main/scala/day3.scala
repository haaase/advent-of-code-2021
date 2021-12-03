import cats.effect.{IO, IOApp}
import java.nio.file.{Path, Paths}
import scala.language.implicitConversions
import cats.kernel.Monoid

object day3 extends IOApp.Simple with AoCSolution {

    // define Bit datatype
    opaque type Bit = Boolean
    object Bit:
        def apply(b: Int): Bit = if (b == 0) then false else true
    extension (b: Bit) def asString: String = if b then "1" else "0"
    given Conversion[Int, Bit] with
        def apply(i: Int): Bit = Bit(i)
    def toDecimal(bitSeq: Seq[Bit]): Int =
        val bitString = bitSeq.map(_.asString)
        Integer.parseInt(Monoid[String].combineAll(bitString), 2)

    def mostCommonBit(bits: Seq[Bit]): Bit =
        val bitCounts = bits.groupBy(identity).view.mapValues(_.size)
        if bitCounts.getOrElse(0, 0) > bitCounts.getOrElse(1, 0) then 0 else 1

    def leastCommonBit(bits: Seq[Bit]): Bit =
        val bitCounts = bits.groupBy(identity).view.mapValues(_.size)
        if bitCounts.getOrElse(0, 0) > bitCounts.getOrElse(1, 0) then 1 else 0

    def rowsToColumns(rows: Seq[String]): Seq[String] =
        val numColumns = 0 to rows(0).size - 1
        rows.foldLeft(numColumns.map(_ => "")) { case (columns, row) =>
            columns.zip(row.toList).map(_ + _)
        }

    // filter a list of bit sequences on a specified criteria
    def filterbyCriteria(
        numbers: List[Seq[Bit]],
        f: (Seq[Bit] => Bit)
    ): Seq[Bit] =
        def findNumber(nums: List[Seq[Bit]], bitPos: Int): Seq[Bit] =
            nums match
                case n :: Nil => n
                case _ =>
                    val relevantBits: List[Bit] = nums.map(_(bitPos))
                    val filterBit = f(relevantBits)
                    val remaining = nums.filter(c => c(bitPos) == filterBit)
                    findNumber(remaining, bitPos + 1)

        findNumber(numbers, 0)

    // parse input and return bit sequences
    def parseInput(input: Seq[String]): List[Seq[Bit]] =
        input.map(_.toSeq.map(c => Bit(c.toString.toInt))).toList

    def getSolution(inputFile: Path): IO[(Int, Int)] =
        for
            input <- read_input(inputFile)
            // part1
            bitColums = parseInput(rowsToColumns(input))
            mostCommonBits = bitColums.map(mostCommonBit(_))
            leastCommonBits = bitColums.map(leastCommonBit(_))
            gammaRate = toDecimal(mostCommonBits)
            epsilonRate = toDecimal(leastCommonBits)
            solution1 = gammaRate * epsilonRate
            // part2
            bitRows = parseInput(input)
            oxygen = toDecimal(filterbyCriteria(bitRows, mostCommonBit))
            co2 = toDecimal(filterbyCriteria(bitRows, leastCommonBit))
            solution2 = oxygen * co2
        yield (solution1, solution2)

    def run =
        for
            solution <- getSolution(Paths.get("inputs/day3.txt"))
            _ <- IO.println(s"Part1: ${solution._1}, Part2: ${solution._2}")
        yield ()
}
