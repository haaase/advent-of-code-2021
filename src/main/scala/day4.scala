import cats.effect.{IO, IOApp}
import java.nio.file.{Path, Paths}
import scala.annotation.tailrec

object day4 extends IOApp.Simple with AoCSolution {

    type Draws = Seq[Int]
    type Field = (Int, Boolean)
    case class BingoBoard(rows: Vector[Vector[Field]]):
        val columns = rows.transpose
        def isWinner =
            // check if we have a winning row
            !rows.filter(_.filterNot(_._2).isEmpty).isEmpty ||
                // check if we have a winning column
                !columns.filter(_.filterNot(_._2).isEmpty).isEmpty

    object BingoBoard:
        def apply(in: Seq[Seq[Int]]): BingoBoard =
            BingoBoard(in.map(_.toVector.map((_, false))).toVector)

    def parseInput(input: Seq[String]): (Draws, List[BingoBoard]) =
        val draws: Draws = input(0).split(",").map(_.toInt)
        val boardLines = input
            .filterNot(_.isEmpty) // trim empty lines
            .tail // drop first line
            // convert to sequences of numbers
            .map(_.split(" ").toSeq.filterNot(_.isEmpty).map(_.toInt))
        // take groups of 5 lines and turn into bingo boards
        val boards = boardLines.grouped(5).map(BingoBoard(_)).toList
        (draws, boards)

    def playRound(boards: Seq[BingoBoard], draw: Int): Seq[BingoBoard] =
        boards.map(board =>
            BingoBoard(
              board.rows.map(fields =>
                  fields.map {
                      case (num, false) if num == draw => (draw, true)
                      case field                       => field
                  }
              )
            )
        )

    @tailrec
    def determineWinner(
        draws: Draws,
        boards: Seq[BingoBoard]
    ): (BingoBoard, Int) =
        val stateOfGame = playRound(boards, draws.head)
        stateOfGame.filter(_.isWinner) match
            case Nil          => determineWinner(draws.tail, stateOfGame)
            case winner :: xs => (winner, draws.head)

    @tailrec
    def determineLastWinner(
        draws: Draws,
        boards: Seq[BingoBoard]
    ): (BingoBoard, Int) =
        val stateOfGame = playRound(boards, draws.head)
        stateOfGame match
            case x :: Nil if x.isWinner =>
                (x, draws.head)
            case _ =>
                determineLastWinner(
                  draws.tail,
                  stateOfGame.filterNot(_.isWinner)
                )

    def calcScore(board: BingoBoard, winningNumber: Int): Int =
        board.rows.flatten.filterNot(_._2).map(_._1).sum * winningNumber

    def getSolution(inputFile: Path): IO[(Int, Int)] =
        for
            input <- read_input(inputFile)
            (draws, boards) = parseInput(input)
            // part1
            (winner, winningNumber) = determineWinner(draws, boards)
            solution1 = calcScore(winner, winningNumber)
            // part 2
            (lastwinner, winningNumber2) = determineLastWinner(draws, boards)
            solution2 = calcScore(lastwinner, winningNumber2)
        yield (solution1, solution2)

    def run =
        for
            solution <- getSolution(Paths.get("inputs/day4.txt"))
            _ <- IO.println(s"Part1: ${solution._1}, Part2: ${solution._2}")
        yield ()
}
