import cats.effect.{IO, IOApp}
import java.nio.file.{Path, Paths}

object day1 extends IOApp.Simple with AoCSolution {
    def buildSlidingWindow(input: List[Int]): List[Int] =
        def windowSum(index: Int) =
            input(index) + input(index + 1) + input(index + 2)
        (0 to input.size - 3).toList.map(windowSum)

    def countIncreases(input: List[Int]): Int =
        input
            .foldLeft(0, input.head) {
                case ((counter, last), num) if num > last =>
                    (counter + 1, num)
                case ((counter, last), num) => (counter, num)
            }
            ._1

    def getSolution(inputFile: Path): IO[(Int, Int)] =
        for
            input <- read_input(inputFile)
            numbers = input.map(_.toInt)
            solution1 = countIncreases(numbers)
            slidingWindow = buildSlidingWindow(numbers)
            solution2 = countIncreases(slidingWindow)
        yield (solution1, solution2)

    def run =
        for
            solution <- getSolution(Paths.get("inputs/day1.txt"))
            _ <- IO.println(s"Part1: ${solution._1}, Part2: ${solution._2}")
        yield ()
}
