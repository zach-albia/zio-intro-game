package net.degoes.zio

import zio.ZIO
import zio.test._
import zio.test.Assertion._
import zio.test.environment._

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
              clearConsole *>
                (for {
                  _                  <- TestConsole.feedLines(name)
                  exitCode           <- PromptName.run(Nil)
                  output             <- TestConsole.output
                  (prompt, greeting) = (output(0), output(1))
                } yield
                  assert(exitCode, equalTo(0)) &&
                    assert(prompt.toLowerCase, isNonEmptyString) &&
                    assert(output.size, equalTo(2)) &&
                    assert(greeting, equalTo(s"Hello, $name!\n")))
          }
        },
        suite("NumberGuesser")(
          testM("correct guess prints congratulatory message") {
            checkM(Gen.int(0, 100)) {
              num =>
                clearConsole *> clearRandom *>
                  (for {
                    _        <- TestRandom.feedInts(num)
                    _        <- TestConsole.feedLines(num.toString)
                    exitCode <- NumberGuesser.run(Nil)
                    output   <- TestConsole.output
                    response = output(1)
                  } yield
                    assert(exitCode, equalTo(0)) &&
                      assert(output.size, equalTo(2)) &&
                      assert(response, equalTo("You guessed correctly!\n")))
            }
          },
          testM("incorrect guess reveals number") {
            val genNumber = Gen.anyInt
            val genNumAndGuess = for {
              num   <- genNumber
              guess <- genNumber if guess != num
            } yield (num, guess)
            checkM(genNumAndGuess) {
              case (num, guess) =>
                clearConsole *> clearRandom *> (for {
                  _        <- TestRandom.feedInts(num)
                  _        <- TestConsole.feedLines(guess.toString)
                  exitCode <- NumberGuesser.run(Nil)
                  output   <- TestConsole.output
                  tokens   = output(1).split(" ").map(_.strip).toList
                } yield
                  assert(exitCode, equalTo(0)) &&
                    assert(tokens, exists(startsWith(num.toString))))
            }
          }
        ),
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
  def clearConsole: ZIO[TestConsole, Nothing, Unit] =
    TestConsole.clearInput *> TestConsole.clearOutput

  def clearRandom: ZIO[TestRandom, Nothing, Unit] =
    TestRandom.clearInts *> TestRandom.clearBooleans *>
      TestRandom.clearBytes *> TestRandom.clearChars *>
      TestRandom.clearDoubles *> TestRandom.clearFloats *>
      TestRandom.clearLongs *> TestRandom.clearStrings
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
