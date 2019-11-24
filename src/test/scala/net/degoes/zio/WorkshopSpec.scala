package net.degoes.zio

import zio.ZIO
import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import zio.test.TestAspect.ignore

object WorkshopSpec
    extends DefaultRunnableSpec({
      import TicTacToe._
      import BoardHelpers._
      import PropertyHelpers._

      suite("Workshop tests")(
        testM("HelloWorld") {
          for {
            exitCode <- HelloWorld.run(Nil)
            output   <- TestConsole.output
          } yield
            assert(exitCode, equalTo(0)) &&
              assert(output, equalTo(Vector("Hello World!\n")))
        },
        testM("ErrorConversion") {
          for {
            exitCode <- ErrorConversion.run(Nil)
            output   <- TestConsole.output
          } yield
            assert(exitCode, equalTo(1)) &&
              assert(output, equalTo(Vector("About to fail...\n", "Uh oh!\n")))
        },
        testM("PromptName") {
          checkM(Gen.alphaNumericString) {
            name =>
              clearConsoleBefore {
                for {
                  _                  <- TestConsole.feedLines(name)
                  exitCode           <- PromptName.run(Nil)
                  output             <- TestConsole.output
                  (prompt, greeting) = (output(0), output(1))
                } yield
                  assert(exitCode, equalTo(0)) &&
                    assert(prompt.toLowerCase, containsString("name")) &&
                    assert(greeting, equalTo(s"Hello, $name!\n"))
              }
          }
        },
        testM("NumberGuesser") {
          val genNumber = Gen.int(0, 100)
          val genGuess = for {
            num   <- genNumber
            guess <- Gen.oneOf(genNumber, Gen.const(num)) // make correct guesses at least 50% of the time
          } yield (num, guess)
          checkM(genGuess) {
            case (num, guess) =>
              clearConsoleBefore {
                for {
                  _                  <- TestRandom.clearInts
                  _                  <- TestRandom.feedInts(num)
                  _                  <- TestConsole.feedLines(guess.toString)
                  exitCode           <- NumberGuesser.run(Nil)
                  output             <- TestConsole.output
                  (prompt, response) = (output(0), output(1))
                } yield
                  assert(exitCode, equalTo(0)) &&
                    assert(prompt, isNonEmptyString) &&
                    assertResponse(num, guess, response)
              }
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

object PropertyHelpers {
  def clearConsoleBefore[R, E](test: ZIO[R, E, TestResult]) =
    clearConsole *> test

  def clearConsole =
    TestConsole.clearInput *> TestConsole.clearOutput

  def assertResponse(num: Int, guess: Int, response: String): TestResult =
    if (num == guess)
      assert(response, equalTo("You guessed correctly!\n"))
    else
      assert(response, containsString(num.toString))

}

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
