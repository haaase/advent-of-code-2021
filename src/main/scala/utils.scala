import cats.effect.IO
import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets

// helper functions for AoC solutions
trait AoCSolution:
  def getSolution(inputFile: Path): IO[(Any, Any)]

  def read_input(path: Path): IO[Seq[String]] =
    for
      string <- IO.blocking(
        String(Files.readAllBytes(path), StandardCharsets.UTF_8)
      )
      result = string.split("\n")
    yield result
