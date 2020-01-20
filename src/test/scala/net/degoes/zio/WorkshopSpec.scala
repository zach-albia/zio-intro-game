package net.degoes.zio

import java.lang.{System => JSystem}

import zio.{Ref, Schedule, ZIO}
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
      import CommonSpecs._
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
                  tokens   = output(1).split(" ").map(_.trim).toList
                } yield
                  assert(exitCode, equalTo(0)) &&
                    assert(tokens, contains(num.toString))
            }
          }
        ),
        suite("AlarmApp")(
          alarmTest(AlarmApp, Int.MaxValue, (out, _) => out.size * 2),
          testAlarmNeverWakesBeforeTime(AlarmApp, maxSleepTime = Int.MaxValue)
        ),
        catSuite(Cat, "Cat"),
        catSuite(CatIncremental, "CatIncremental"),
        suite("AlarmAppImproved")(
          alarmTest(
            AlarmAppImproved,
            3600, // an hour's worth of dots; much more and tests run for too long
            (out, duration) => (out.size * 2) + duration
          ),
          testAlarmNeverWakesBeforeTime(AlarmAppImproved, maxSleepTime = 3600)
        ),
        suite("ComputePi")(
          suite("runSimulation")(
            testM("inside <= total") {
              checkM(Gen.int(0, 10000)) { sampleSize =>
                for {
                  state       <- ComputePi.initState
                  concurrency <- ZIO.effectTotal(java.lang.Runtime.getRuntime.availableProcessors)
                  _           <- ComputePi.runSimulation(sampleSize, concurrency, state)
                  inside      <- state.inside.get
                  total       <- state.total.get
                } yield assert(inside, isLessThanEqualTo(total))
              }
            },
            testM("estimates tend to get better with more samples") {
              checkM(Gen.int(1, 100) <*> Gen.int(500, 1000)) {
                case (fewerSamples, moreSamples) =>
                  val timesToTry = 30

                  import ComputePi._
                  import java.lang.Runtime.getRuntime

                  def runSample =
                    for {
                      fewerSamplesState <- initState
                      concurrency       <- ZIO.effectTotal(getRuntime.availableProcessors)
                      _                 <- runSimulation(fewerSamples, concurrency, fewerSamplesState)
                      moreSamplesState  <- initState
                      _                 <- runSimulation(moreSamples, concurrency, moreSamplesState)
                      fewerSamplesPi    <- currentEstimate(fewerSamplesState)
                      moreSamplesPi     <- currentEstimate(moreSamplesState)
                    } yield (fewerSamplesPi, moreSamplesPi)

                  def checkWinners(moreWins: Ref[Long], lessWins: Ref[Long]) =
                    for {
                      estimates         <- runSample
                      (fewerPi, morePi) = estimates
                      _ <- if (math.abs(math.Pi - morePi) <= math.abs(math.Pi - fewerPi))
                            moreWins.update(_ + 1)
                          else
                            lessWins.update(_ + 1)
                    } yield ()

                  for {
                    moreWinCount <- Ref.make(0L)
                    lessWinCount <- Ref.make(0L)
                    _ <- checkWinners(moreWinCount, lessWinCount).repeat(
                          Schedule.recurs(timesToTry - 1))
                    moreWins <- moreWinCount.get
                    lessWins <- lessWinCount.get
                  } yield assert(moreWins, isGreaterThan(lessWins))
              }
            }
          )
        ),
        suite("StmSwap")(
          suite("exampleRef is non-deterministic")(
            testM("can be 0") {
              assertM(StmSwap.exampleRef, equalTo(0))
            } @@ TestAspect.flaky,
            testM("can be 100") {
              assertM(StmSwap.exampleRef, equalTo(100))
            } @@ TestAspect.flaky,
            testM("can be 200") {
              assertM(StmSwap.exampleRef, equalTo(200))
            } @@ TestAspect.flaky
          ),
          testM("exampleStm is deterministic") {
            assertM(StmSwap.exampleStm, equalTo(100))
          } @@ TestAspect.nonFlaky
        ),
        suite("StmLock")(
          testM("prints 11 of 1 message and then 11 of another") {
            for {
              _                       <- clearConsole
              exitCode                <- StmLock.run(Nil)
              output                  <- TestConsole.output
              (firstHalf, secondHalf) = output.splitAt(11)
            } yield
              assert(exitCode, equalTo(0)) &&
                firstHalf.map(a => assert(a, equalTo(firstHalf.head))).reduce(_ && _) &&
                secondHalf.map(a => assert(a, equalTo(secondHalf.head))).reduce(_ && _) &&
                assert(firstHalf.head, not(equalTo(secondHalf.head)))
          } @@ TestAspect.nonFlaky
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

object CommonSpecs {
  import PropertyHelpers._

  val lineSep: String = JSystem.lineSeparator

  def catSuite(cat: zio.App, label: String) =
    suite(label)(
      testM("prints string read from file") {
        val contents = Gen.vectorOf(Gen.string(Gen.printableChar).filter(_.nonEmpty))
        checkM(contents) {
          contentsWrittenToFile =>
            for {
              _         <- clearConsole
              tempFile  <- Files.createTempFile(".tmp", None, List.empty)
              _         <- Files.writeLines(tempFile, contentsWrittenToFile)
              absPath   <- tempFile.toAbsolutePath
              exitCode  <- cat.run(List(absPath.toString))
              output    <- TestConsole.output
              exists    <- Files.deleteIfExists(tempFile)
              expected  = contentsWrittenToFile.map(_ + lineSep)
              outputVec = output.mkString.split(s"(?<=$lineSep)").toVector
              actual = outputVec.lastOption
                .fold(outputVec)(last => if (last == "\n") outputVec.dropRight(1) else outputVec)
                .filter(_.nonEmpty)
            } yield
              assert(exitCode, equalTo(0)) &&
                assert(expected, equalTo(actual)) &&
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
      } @@ TestAspect.flaky // On the off-chance files get messed up
    )

  def alarmTest(
      alarmApp: zio.App,
      maxSleepTime: Int,
      expectedOutputSize: (Vector[String], Int) => Int
  ) =
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
              assert(exitCode, equalTo(0)) && // always succeeds because of retry logic
                assert(output.size, equalTo(expected)) // lines printed = prompts + 1 wake message
          }
      }
    }

  def testAlarmNeverWakesBeforeTime(
      app: zio.App,
      maxSleepTime: Int
  ): ZSpec[zio.ZEnv with TestConsole, Nothing, String, Unit] = {
    testM("Never wakes up before alarm goes off") {
      val times = for {
        sleepTime   <- Gen.int(1, maxSleepTime)
        beforeAlarm <- Gen.int(0, sleepTime - 1)
      } yield (sleepTime, beforeAlarm)
      checkM(times) {
        case (sleepTime, beforeAlarm) =>
          resetClock {
            for {
              _     <- clearConsole
              _     <- TestConsole.feedLines(sleepTime.toString)
              _     <- TestClock.adjust(beforeAlarm.seconds)
              fiber <- app.run(Nil).fork
              _     <- TestClock.sleeps.doUntil(_.nonEmpty)
              exit  <- fiber.interrupt
            } yield assert(exit, isInterrupted)
          }
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
          val console   = env.console
          val blocking  = env.blocking
          val random    = env.random
          val system    = env.system
          val clock     = newClock.clock
          val scheduler = newClock.scheduler
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
