package net.degoes.zio

import zio.ZIO
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.duration._
import zio.random.Random
import zio.system.System
import zio.test.Assertion._
import zio.test._
import zio.test.environment._
import zio.test.TestAspect.ignore

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
          } yield assert(exitCode, equalTo(0)) && assert(output, hasSize(equalTo(1)))
        },
        testM("PrintSequence") {
          for {
            exitCode <- PrintSequence.run(Nil)
            output   <- TestConsole.output
          } yield assert(exitCode, equalTo(0)) && assert(output, hasSize(equalTo(3)))
        },
        testM("ErrorRecovery") {
          for {
            exitCode <- ErrorRecovery.run(Nil)
            output   <- TestConsole.output
          } yield assert(exitCode, equalTo(1)) && assert(output.size, equalTo(2))
        },
        testM("Looping") {
          for {
            exitCode <- Looping.run(Nil)
            output   <- TestConsole.output
          } yield assert(exitCode, equalTo(0)) && assert(output.size, equalTo(100))
        },
        testM("EffectConversion") {
          assertM(EffectConversion.run(Nil), equalTo(0))
        },
        testM("PromptName greets with name") {
          checkM(Gen.alphaNumericString) {
            name =>
              for {
                _                  <- clearConsole
                _                  <- TestConsole.feedLines(name)
                exitCode           <- PromptName.run(Nil)
                output             <- TestConsole.output
                (prompt, greeting) = (output(0), output(1))
              } yield
                assert(exitCode, equalTo(0)) &&
                  assert(prompt.toLowerCase, isNonEmptyString) &&
                  assert(output.size, equalTo(2)) &&
                  assert(greeting, equalTo(s"Hello, $name!\n"))
          }
        },
        suite("NumberGuesser")(
          testM("correct guess prints congratulatory message") {
            checkM(Gen.int(0, 100)) { num =>
              for {
                _        <- clearConsole *> clearRandom
                _        <- TestRandom.feedInts(num)
                _        <- TestConsole.feedLines(num.toString)
                exitCode <- NumberGuesser.run(Nil)
                output   <- TestConsole.output
                response = output(1)
              } yield
                assert(exitCode, equalTo(0)) &&
                  assert(output.size, equalTo(2)) &&
                  assert(response, equalTo("You guessed correctly!\n"))
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
                for {
                  _        <- clearConsole *> clearRandom
                  _        <- TestRandom.feedInts(num)
                  _        <- TestConsole.feedLines(guess.toString)
                  exitCode <- NumberGuesser.run(Nil)
                  output   <- TestConsole.output
                  tokens   = output(1).split(" ").map(_.strip).toList
                } yield
                  assert(exitCode, equalTo(0)) &&
                    assert(tokens, contains(num.toString))
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
                resetClock {
                  for {
                    _        <- clearConsole
                    _        <- TestConsole.feedLines(tries: _*)
                    _        <- TestClock.adjust(goodTry.toInt.seconds)
                    exitCode <- AlarmApp.run(Nil)
                    output   <- TestConsole.output
                  } yield
                    assert(exitCode, equalTo(0)) && // program always succeeds because of retry logic
                      assert(output.size, equalTo(tries.size + 1)) // lines printed to console = prompts + 1 wake message
                }
            }
          },
          testM("Never wakes up before alarm goes off") {
            val times = for {
              sleepTime   <- Gen.int(1, Int.MaxValue)
              beforeAlarm <- Gen.int(0, sleepTime - 1)
            } yield (sleepTime, beforeAlarm)
            checkM(times) {
              case (sleepTime, beforeAlarm) =>
                resetClock {
                  for {
                    _      <- clearConsole
                    _      <- TestConsole.feedLines(sleepTime.toString)
                    _      <- TestClock.adjust(beforeAlarm.seconds)
                    fiber  <- AlarmApp.run(Nil).fork
                    _      <- waitForSleep()
                    exit   <- fiber.interrupt
                    output <- TestConsole.output
                  } yield
                    assert(exit, isInterrupted) && // alarm has not gone off
                      assert(output.size, equalTo(1)) // only prompt message written
                }
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

  def resetClock(test: ZIO[TestConsole with TestClock with zio.ZEnv, Nothing, TestResult]) =
    test.provideSomeManaged {
      for {
        env      <- ZIO.environment[zio.ZEnv with TestConsole].toManaged_
        newClock <- TestClock.make(TestClock.DefaultData)
        testEnv = new TestConsole with TestClock with Clock with Console with System with Random
        with Blocking {
          override val console: TestConsole.Service[Any] = env.console
          override val blocking: Blocking.Service[Any]   = env.blocking
          override val random: Random.Service[Any]       = env.random
          override val system: System.Service[Any]       = env.system
          override val clock: TestClock.Service[Any]     = newClock.clock
          override val scheduler: TestClock.Service[Any] = newClock.scheduler
        }
      } yield testEnv
    }

  def waitForSleep(): ZIO[TestClock, Nothing, Unit] =
    TestClock.sleeps.flatMap { sleeps =>
      if (sleeps.nonEmpty) ZIO.unit else waitForSleep()
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
