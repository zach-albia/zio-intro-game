package net.degoes.zio

import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import zio.test.TestAspect.ignore

object WorkshopSpec
    extends DefaultRunnableSpec({
      import TicTacToe._
      import BoardHelpers._

      suite("Workshop tests")(
        testM("HelloWorld") {
          for {
            value  <- HelloWorld.run(Nil)
            output <- TestConsole.output
          } yield
            assert(value, equalTo(0)) &&
              assert(output, equalTo(Vector("Hello World!\n")))
        },
        testM("ErrorConversion") {
          for {
            value  <- ErrorConversion.run(Nil)
            output <- TestConsole.output
          } yield
            assert(value, equalTo(1)) &&
              assert(output, equalTo(Vector("About to fail...\n", "Uh oh!\n")))
        },
        testM("PromptName") {
          val nameGen = Gen
            .listOf(Gen.alphaNumericChar)
            .filter(_.nonEmpty)
            .map(_.mkString)
          checkM(nameGen) {
            name =>
              for {
                // gotta clear the console before each run or the current lines
                // carry over to the other runs!
                _                  <- TestConsole.clearInput
                _                  <- TestConsole.clearOutput
                _                  <- TestConsole.feedLines(name)
                exitCode           <- PromptName.run(Nil)
                output             <- TestConsole.output
                (prompt, greeting) = (output(0), output(1))
              } yield
                assert(exitCode, equalTo(0)) &&
                  assert(prompt.toLowerCase, containsString("name")) &&
                  assert(greeting, equalTo(s"Hello, $name!\n"))
          }
        },
        suite("Board")(
          test("won horizontal first") {
            horizontalFirst(Mark.X) && horizontalFirst(Mark.O)
          },
          test("won horizontal second") {
            horizontalSecond(Mark.X) && horizontalSecond(Mark.O)
          },
          test("won horizontal third") {
            horizontalThird(Mark.X) && horizontalThird(Mark.O)
          },
          test("won vertical first") {
            verticalFirst(Mark.X) && verticalFirst(Mark.O)
          },
          test("won vertical second") {
            verticalSecond(Mark.X) && verticalSecond(Mark.O)
          },
          test("won vertical third") {
            verticalThird(Mark.X) && verticalThird(Mark.O)
          },
          test("won diagonal first") {
            diagonalFirst(Mark.X) && diagonalFirst(Mark.O)
          },
          test("won diagonal second") {
            diagonalSecond(Mark.X) && diagonalSecond(Mark.O)
          }
        )
      )
    })

object BoardHelpers {
  import TicTacToe._

  def horizontalFirst(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(chr, chr, chr),
          List(' ', ' ', ' '),
          List(' ', ' ', ' ')
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }

  def horizontalSecond(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(' ', ' ', ' '),
          List(chr, chr, chr),
          List(' ', ' ', ' ')
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }

  def horizontalThird(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(' ', ' ', ' '),
          List(' ', ' ', ' '),
          List(chr, chr, chr)
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }

  def verticalFirst(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(chr, ' ', ' '),
          List(chr, ' ', ' '),
          List(chr, ' ', ' ')
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }

  def verticalSecond(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(' ', chr, ' '),
          List(' ', chr, ' '),
          List(' ', chr, ' ')
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }

  def verticalThird(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(' ', ' ', chr),
          List(' ', ' ', chr),
          List(' ', ' ', chr)
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }

  def diagonalFirst(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(' ', ' ', chr),
          List(' ', chr, ' '),
          List(chr, ' ', ' ')
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }

  def diagonalSecond(mark: Mark) = {
    val chr = mark.renderChar

    assert(
      Board
        .fromChars(
          List(chr, ' ', ' '),
          List(' ', chr, ' '),
          List(' ', ' ', chr)
        )
        .flatMap(_.won),
      isSome(equalTo(mark))
    )
  }
}
