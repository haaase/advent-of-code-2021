import com.monovore.decline._
import java.nio.file.{Path}
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object CLI extends IOApp:
    val dayOpt: Opts[Int] = Opts.argument[Int](metavar = "DAY")
    val fileOpt: Opts[Path] = Opts.argument[Path](metavar = "INPUT-FILE")
    // program
    val aocCommand = Command(
      name = "aoc-21",
      header = "My solutions for Advent of Code 2021."
    ) {
        (dayOpt, fileOpt).tupled
    }
    def run(args: List[String]): IO[ExitCode] =
        // parse arguments
        val commands = aocCommand.parse(args)
        commands.match
                // --help flag given
                case Left(help: Help) if help.errors.isEmpty =>
                    IO.println(help).as(ExitCode.Success)
                    // errors occured
                case Left(err) => IO.println(err).as(ExitCode.Error)
                case Right((day: Int, path: Path)) =>
                    // determine program to run
                    val prog = day match
                        case 1 => day1.getSolution(path).attempt
                        case 2 => day2.getSolution(path).attempt
                        case 3 => day3.getSolution(path).attempt
                        case 4 => day4.getSolution(path).attempt
                        case _ =>
                            IO.pure(
                              Left(
                                new IllegalArgumentException(
                                  s"Day $day is either invalid or not implemented yet!"
                                )
                              )
                            )
                    for
                        // run program and check result
                        solution <- prog
                        res <- solution match
                            case Left(err) => IO.println(err).as(ExitCode.Error)
                            case Right((r1, r2)) =>
                                IO.println(s"Part1: $r1, Part2: $r2")
                                    .as(ExitCode.Success)
                    yield res
