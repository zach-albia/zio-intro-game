package net.degoes.zio

import zio.ZIO
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.duration._
import zio.nio.file.{Files, Path}
import zio.random.Random
import zio.system.System
import zio.test.Assertion._
import zio.test._
import zio.test.environment._

object WorkshopSpec
    extends DefaultRunnableSpec({
      import BoardHelpers._
      import PropertyHelpers._
      import Suites._
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
          alarmTest(AlarmApp, Int.MaxValue, (out, _) => out.size * 2),
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
                    _      <- TestClock.sleeps.doUntil(_.nonEmpty)
                    exit   <- fiber.interrupt
                    output <- TestConsole.output
                  } yield
                    assert(exit, isInterrupted) && // alarm has not gone off
                      assert(output.size, equalTo(1)) // only prompt message written
                }
            }
          }
        ),
        catSuite(Cat, "Cat"),
        catSuite(CatIncremental, "CatIncremental"),
        suite("AlarmAppImproved")(
          alarmTest(
            AlarmAppImproved,
            3600, // an hour's worth of dots, too many and tests run slow
            (out, duration) => (out.size * 2) + duration
          ),
          testM("Never wakes up before alarm goes off") {
            for {
              _        <- TestConsole.feedLines("5")
              _        <- TestClock.adjust(3.seconds)
              fiber    <- AlarmAppImproved.run(Nil).fork
              _        <- TestClock.sleeps.doUntil(_.nonEmpty)
              exit     <- fiber.interrupt
              output   <- TestConsole.output
              expected = 1 + 4
            } yield assert(exit, isInterrupted) && assert(output.size, equalTo(expected))
          } @@ TestAspect.timeout(5.seconds)
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

object Suites {
  import PropertyHelpers._

  def catSuite(cat: zio.App, label: String) =
    suite(label)(
      testM("prints string read from file") {
        val contents = Gen.vectorOf(Gen.string(Gen.printableChar).filter(_.nonEmpty))
        checkM(contents) {
          contents =>
            for {
              _        <- clearConsole
              tempFile <- Files.createTempFile(".tmp", None, List.empty)
              _        <- Files.writeLines(tempFile, contents)
              absPath  <- tempFile.toAbsolutePath
              exitCode <- cat.run(List(absPath.toString))
              output   <- TestConsole.output
              exists   <- Files.deleteIfExists(tempFile)
              lines    = output.mkString.split("\\s+").toVector.filter(_.nonEmpty)
            } yield
              assert(exitCode, equalTo(0)) &&
                assert(lines, equalTo(contents)) &&
                assert(exists, isTrue)
        }
      },
      testM("prints usage when no path given") {
        assertM(cat.run(Nil), equalTo(2))
      },
      testM("prints usage when more than one path given") {
        checkM(Gen.listOf(Gen.const("a")).filter(_.size > 1)) { list =>
          assertM(cat.run(list), equalTo(2))
        }
      },
      testM("fails when given path to nonexistent file") {
        import zio.random._

        def makeNonExistentPath(): ZIO[Blocking with Random, Nothing, Path] =
          for {
            length          <- nextInt(12).map(_ + 8)
            str             <- nextString(length)
            path            = Path(str)
            exists          <- Files.exists(path)
            nonExistentPath <- if (exists) makeNonExistentPath() else ZIO.succeed(path)
          } yield nonExistentPath

        for {
          path     <- makeNonExistentPath()
          exitCode <- cat.run(List(path.toString))
        } yield assert(exitCode, equalTo(1))
      } @@ TestAspect.flaky
    )

  def alarmTest(alarmApp: zio.App,
                maxSleepTime: Int,
                expectedOutputSize: (Vector[String], Int) => Int) =
    testM("Retries until good input given then wakes up") {
      val tries = for {
        badInputs <- Gen.listOf(Gen.alphaNumericString.filter(_.forall(!_.isDigit)))
        goodInput <- Gen.int(1, maxSleepTime).map(_.toString)
      } yield (badInputs.toVector :+ goodInput, goodInput)
      checkM(tries) {
        case (tries, goodTry) =>
          resetClock {
            for {
              _        <- clearConsole
              _        <- TestConsole.feedLines(tries: _*)
              duration = goodTry.toInt
              _        <- TestClock.adjust(duration.seconds)
              exitCode <- alarmApp.run(Nil)
              output   <- TestConsole.output
              expected = expectedOutputSize(tries, duration)
            } yield
              assert(exitCode, equalTo(0)) && // program always succeeds because of retry logic
                assert(output.size, equalTo(expected)) // lines printed to console = prompts + 1 wake message
          }
      }
    }
}

object PropertyHelpers {
  trait WorkshopTestEnvironment
      extends TestConsole
      with TestClock
      with Clock
      with Console
      with System
      with Random
      with Blocking

  def clearConsole =
    TestConsole.clearInput *> TestConsole.clearOutput

  def clearRandom =
    TestRandom.clearInts *> TestRandom.clearBooleans *>
      TestRandom.clearBytes *> TestRandom.clearChars *>
      TestRandom.clearDoubles *> TestRandom.clearFloats *>
      TestRandom.clearLongs *> TestRandom.clearStrings

  def resetClock(test: ZIO[WorkshopTestEnvironment, Nothing, TestResult]) =
    test.provideSomeManaged {
      for {
        env      <- ZIO.environment[zio.ZEnv with TestConsole].toManaged_
        newClock <- TestClock.make(TestClock.DefaultData)
        testEnv = new WorkshopTestEnvironment {
          override val console: TestConsole.Service[Any] = env.console
          override val blocking: Blocking.Service[Any]   = env.blocking
          override val random: Random.Service[Any]       = env.random
          override val system: System.Service[Any]       = env.system
          override val clock: TestClock.Service[Any]     = newClock.clock
          override val scheduler: TestClock.Service[Any] = newClock.scheduler
        }
      } yield testEnv
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
