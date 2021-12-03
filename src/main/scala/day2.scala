import cats.effect.{IO, IOApp}
import java.nio.file.{Path, Paths}
import cats.implicits

object day2 extends IOApp.Simple with AoCSolution {

    case class Position(x: Int, y: Int, aim: Int)

    enum Step:
        case Forward(amount: Int) extends Step
        case Down(amount: Int) extends Step
        case Up(amount: Int) extends Step

    def parseInput(input: List[String]): List[Step] =
        def parseStep(step: String) =
            step.split(" ").toList match
                case List("forward", x) => Step.Forward(x.toInt)
                case List("down", x)    => Step.Down(x.toInt)
                case List("up", x)      => Step.Up(x.toInt)
                case i =>
                    throw new IllegalArgumentException(s"Invalid input: $i")
        input.map(parseStep)

    def calcPosition1(currPos: Position, steps: List[Step]): Position =
        steps match
            case Nil => currPos
            case Step.Forward(amount) :: xs =>
                calcPosition1(currPos.copy(x = currPos.x + amount), xs)
            case Step.Down(amount) :: xs =>
                calcPosition1(currPos.copy(y = currPos.y + amount), xs)
            case Step.Up(amount) :: xs =>
                calcPosition1(currPos.copy(y = currPos.y - amount), xs)

    def calcPosition2(currPos: Position, steps: List[Step]): Position =
        steps match
            case Nil => currPos
            case Step.Forward(amount) :: xs =>
                calcPosition2(
                  currPos.copy(
                    x = currPos.x + amount,
                    y = currPos.y + currPos.aim * amount
                  ),
                  xs
                )
            case Step.Down(amount) :: xs =>
                calcPosition2(currPos.copy(aim = currPos.aim + amount), xs)
            case Step.Up(amount) :: xs =>
                calcPosition2(currPos.copy(aim = currPos.aim - amount), xs)

    def getSolution(inputFile: Path): IO[(Int, Int)] =
        for
            input <- read_input(inputFile).map(_.toList)
            steps = parseInput(input)
            position1 = calcPosition1(Position(0, 0, 0), steps)
            solution1 = position1.x * position1.y
            position2 = calcPosition2(Position(0, 0, 0), steps)
            solution2 = position2.x * position2.y
        yield (solution1, solution2)

    def run =
        for
            solution <- getSolution(Paths.get("inputs/day2.txt"))
            _ <- IO.println(s"Part1: ${solution._1}, Part2: ${solution._2}")
        yield ()
}
