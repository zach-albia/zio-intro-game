package net.degoes.zio

import zio.ZIO
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.duration._
import zio.random.Random
import zio.system.System
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._
import zio.test.environment._

object WorkshopSpec
    extends DefaultRunnableSpec({
      import BoardHelpers._
      import PropertyHelpers._
      import TicTacToe._

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
        testM("PromptName greets with name") {
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
            checkM(Gen.int(0, 100)) { num =>
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
        suite("AlarmApp")(
          testM("Retries until good input given then wakes up") {
            val tries = for {
              badInputs <- Gen.listOf(Gen.alphaNumericString.filter(_.forall(!_.isDigit)))
              goodInput <- Gen.int(1, Int.MaxValue).map(_.toString)
            } yield (badInputs.toVector :+ goodInput, goodInput)
            checkM(tries) {
              case (tries, goodTry) =>
                clearConsole *> (for {
                  _        <- TestConsole.feedLines(tries: _*)
                  _        <- TestClock.adjust(goodTry.toInt.seconds)
                  exitCode <- AlarmApp.run(Nil)
                  output   <- TestConsole.output
                } yield
                // program always succeeds because of retry logic
                assert(exitCode, equalTo(0)) &&
                  // lines printed to console = prompts + wake message
                  assert(output.size, equalTo(tries.size + 1)) &&
                  assert(output.last.strip, equalTo("Wake up!")))
                  .provideSomeManaged(newTestClock)
            }
          },
          testM("Never wakes up before alarm goes off") {
            val times = for {
              sleepTime   <- Gen.int(2, Int.MaxValue)
              beforeAlarm <- Gen.int(1, sleepTime - 1)
            } yield (sleepTime, beforeAlarm)
            checkM(times) {
              case (sleepTime, beforeAlarm) =>
                clearConsole *> (for {
                  _      <- TestConsole.feedLines(sleepTime.toString)
                  _      <- TestClock.adjust(beforeAlarm.seconds)
                  fiber  <- AlarmApp.run(Nil).fork
                  _      <- TestClock.adjust(0.seconds) // RACE CONDITIONNNNNN
                  exit   <- fiber.interrupt // To fix RACE CONDITION, we have to wait for clock to sleep before we kill the fiber
                  output <- TestConsole.output
                } yield
                // assert alarm has not gone off
                assert(exit, isInterrupted) &&
                  // only prompt message was written to console
                  assert(output.size, equalTo(1)))
                  .provideSomeManaged(newTestClock)
            }
          }
        ) @@ timeout(10.seconds),
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

  def newTestClock = {
    for {
      env     <- ZIO.environment[zio.ZEnv with TestConsole].toManaged_
      clock   <- TestClock.make(TestClock.DefaultData)
      testEnv = useNewClock(env, clock)
    } yield testEnv
  }

  def useNewClock(env: zio.ZEnv with TestConsole, testClock: TestClock) = {
    new TestConsole with TestClock with Clock with Console with System with Random with Blocking {
      override val console: TestConsole.Service[Any] = env.console
      override val blocking: Blocking.Service[Any]   = env.blocking
      override val random: Random.Service[Any]       = env.random
      override val system: System.Service[Any]       = env.system
      override val clock: TestClock.Service[Any]     = testClock.clock
      override val scheduler: TestClock.Service[Any] = testClock.scheduler
    }
  }
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
