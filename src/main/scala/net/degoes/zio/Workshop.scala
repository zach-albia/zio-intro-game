package net.degoes.zio

import zio._
import java.text.NumberFormat

object ZIOTypes {

  /**
    * EXERCISE 1
    *
    * Provide definitions for the ZIO type aliases below.
    */
  type Task[+A]     = ZIO[Any, Throwable, A]
  type UIO[+A]      = ZIO[Any, Nothing, A]
  type RIO[-R, +A]  = ZIO[R, Throwable, A]
  type IO[+E, +A]   = ZIO[Any, E, A]
  type URIO[-R, +A] = ZIO[R, Nothing, A]
}

import scala.io.{BufferedSource, Source}

object HelloWorld extends App {

  import zio.console._

  /**
    * EXERCISE 2
    *
    * Implement a simple "Hello World!" program using the effect returned by `putStrLn`.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn("Hello World!") as 0
}

object PrintSequence extends App {
  import zio.console._

  /**
    * EXERCISE 3
    *
    * Using `*>` (`zipRight`), compose a sequence of `putStrLn` effects to
    * produce an effect that prints three lines of text to the console.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn("quas") *> putStrLn("wex") *> putStrLn("exort") as 0
}

object ErrorRecovery extends App {
  val StdInputFailed = 1

  import zio.console._

  val failed =
    putStrLn("About to fail...") *>
      ZIO.fail("Uh oh!") *>
      putStrLn("This will NEVER be printed!")

  /**
    * EXERCISE 4
    *
    * Using `ZIO#orElse` or `ZIO#fold`, have the `run` function compose the
    * preceding `failed` effect into the effect that `run` returns.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    failed.foldM(s => putStrLn(s) *> ZIO.succeed(1), _ => ZIO.succeed(0))
}

object Looping extends App {
  import zio.console._

  /**
    * EXERCISE 5
    *
    * Implement a `repeat` combinator using `flatMap` and recursion.
    */
  def repeat[R, E, A](n: Int)(task: ZIO[R, E, A]): ZIO[R, E, A] =
    if (n > 1) task.flatMap(_ => repeat(n - 1)(task)) else task

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    repeat(100)(putStrLn("All work and no play makes Jack a dull boy")) as 0
}

object EffectConversion extends App {

  /**
    * EXERCISE 6
    *
    * Using ZIO.effect, convert the side-effecting of `println` into a pure
    * functional effect.
    */
  def myPrintLn(line: String): Task[Unit] = ZIO.effect(println(line))

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (myPrintLn("Hello Again!") as 0) orElse ZIO.succeed(1)
}

object ErrorNarrowing extends App {
  import java.io.IOException

  import scala.io.StdIn.readLine
  implicit class Unimplemented[A](v: A) {
    def ? = ???
  }

  /**
    * EXERCISE 7
    *
    * Using `ZIO#refineToOrDie`, narrow the error type of the following
    * effect to IOException.
    */
  val myReadLine: IO[IOException, String] = ZIO.effect(readLine()).refineToOrDie[IOException]

  def myPrintLn(line: String): UIO[Unit] = UIO(println(line))

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _    <- myPrintLn("What is your name?")
      name <- myReadLine
      _    <- myPrintLn(s"Good to meet you, ${name}")
    } yield 0) orElse ZIO.succeed(1)
}

object PromptName extends App {
  val StdInputFailed = 1

  import zio.console._

  /**
    * EXERCISE 8
    *
    * Using `ZIO#flatMap`, implement a simple program that asks the user for
    * their name (using `getStrLn`), and then prints it out to the user (using `putStrLn`).
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _    <- putStrLn("What is your name?")
      name <- getStrLn
      _    <- putStrLn(s"Hello, $name!")
    } yield ())
      .foldM(e => putStrLn(e.getMessage) *> ZIO.succeed(1), _ => ZIO.succeed(0))
}

object NumberGuesser extends App {

  import zio.console._
  import zio.random._

  def analyzeAnswer(random: Int, guess: String) =
    if (random.toString == guess.trim) putStrLn("You guessed correctly!")
    else putStrLn(s"You did not guess correctly. The answer was $random")

  /**
    * EXERCISE 9
    *
    * Choose a random number (using `nextInt`), and then ask the user to guess
    * the number, feeding their response to `analyzeAnswer`, above.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    (for {
      _       <- putStr("Please enter an integer from 0 to 100: ")
      randInt <- nextInt(100)
      guess   <- getStrLn
      _       <- analyzeAnswer(randInt, guess)
    } yield 0) orElse ZIO.succeed(1)
  }
}

object AlarmApp extends App {

  import java.io.IOException

  import zio.console._
  import zio.duration._

  /**
    * EXERCISE 10
    *
    * Create an effect that will get a `Duration` from the user, by prompting
    * the user to enter a decimal number of seconds. Use `refineOrDie` to
    * narrow the error type as necessary.
    */
  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO.effect(input.toInt.seconds).refineToOrDie[NumberFormatException]

    def fallback(input: String): ZIO[Console, IOException, Duration] =
      parseDuration(input) orElse putStrLn("You didn't enter the number of seconds!") *> getAlarmDuration

    for {
      _        <- putStrLn("Please enter the number of seconds to sleep: ")
      input    <- getStrLn
      duration <- parseDuration(input) orElse fallback(input)
    } yield duration
  }

  /**
    * EXERCISE 11
    *
    * Create a program that asks the user for a number of seconds to sleep,
    * sleeps the specified number of seconds using ZIO.sleep(d), and then
    * prints out a wakeup alarm message, like "Time to wakeup!!!".
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      duration <- getAlarmDuration
      _        <- clock.sleep(duration)
      _        <- putStrLn("Wake up!")
    } yield ())
      .foldM(e => putStrLn(e.getMessage) *> ZIO.succeed(1), _ => ZIO.succeed(0))
}

object Cat extends App {

  import java.io.IOException

  import zio.blocking._
  import zio.console._

  private def openFile(file: String) =
    blocking(ZIO.effect(Source.fromFile(file))).refineToOrDie[IOException]

  private def closeFile(bs: BufferedSource) =
    blocking(ZIO.effect(bs.close()).orDie)

  /**
    * EXERCISE 12
    *
    * Implement a function to read a file on the blocking thread pool, storing
    * the result into a string.
    */
  def readFile(file: String): ZIO[Blocking, IOException, String] =
    openFile(file).bracket(closeFile, bs => ZIO.succeed(bs.mkString))

  /**
    * EXERCISE 13
    *
    * Implement a version of the command-line utility "cat", which dumps the
    * contents of the specified file to standard output.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    args match {
      case file :: Nil =>
        (for {
          contents <- readFile(file)
          _        <- putStrLn(contents)
        } yield 0)
          .foldM(e => putStrLn(e.getMessage) *> ZIO.succeed(1), _ => ZIO.succeed(0))
      case _ => putStrLn("Usage: cat <file>") as 2
    }
}

object CatIncremental extends App {

  import java.io.{FileInputStream, IOException, InputStream, Console => _}

  import zio.blocking._
  import zio.console._

  val CHUNK_SIZE = 1024

  /**
    * BONUS EXERCISE
    *
    * Implement a `blockingIO` combinator to use in subsequent exercises.
    */
  def blockingIO[A](a: => A): ZIO[Blocking, IOException, A] =
    effectBlocking(a).refineToOrDie[IOException]

  /**
    * EXERCISE 14
    *
    * Implement all missing methods of `FileHandle`. Be sure to do all work on
    * the blocking thread pool.
    */
  final case class FileHandle private (private val is: InputStream) {
    final def close: ZIO[Blocking, IOException, Unit] = blockingIO(is.close())

    final def read: ZIO[Blocking, IOException, Option[Chunk[Byte]]] =
      blockingIO {
        if (is.available() == 0) None
        else { // here be mutation!
          val arr               = Array.ofDim[Byte](CHUNK_SIZE)
          val bytesRead         = is.read(arr)
          var copy: Array[Byte] = Array.ofDim[Byte](bytesRead)
          if (bytesRead != arr.length) Array.copy(arr, 0, copy, 0, bytesRead)
          else copy = arr
          if (bytesRead == -1) None else Some(Chunk.fromArray(copy))
        }
      }
  }

  object FileHandle {
    final def open(file: String): ZIO[Blocking, IOException, FileHandle] =
      blockingIO(FileHandle(new FileInputStream(file)))
  }

  /**
    * EXERCISE 15
    *
    * Implement an incremental version of the `cat` utility, using `ZIO#bracket`
    * or `ZManaged` to ensure the file is closed in the event of error or
    * interruption.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    args match {
      case file :: Nil =>
        cat(file).foldM(e => putStrLn(e.getMessage) *> ZIO.succeed(1), _ => ZIO.succeed(0))
      case _ => putStrLn("Usage: cat <file>") as 2
    }

  def cat(file: String): ZIO[Console with Blocking, IOException, Unit] =
    ZManaged.make(FileHandle.open(file))(_.close.orDie).use(read)

  def read(fileHandle: FileHandle): ZIO[Console with Blocking, IOException, Unit] =
    for {
      opt <- fileHandle.read
      _ <- opt
            .map(chunk => putStr(chunk.map(_.toChar).mkString) *> read(fileHandle))
            .getOrElse(ZIO.unit)
    } yield ()
}

object AlarmAppImproved extends App {
  import java.io.IOException
  import java.util.concurrent.TimeUnit

  import zio.console._
  import zio.duration._

  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO
        .effect(
          Duration((input.toDouble * 1000.0).toLong, TimeUnit.MILLISECONDS)
        )
        .refineToOrDie[NumberFormatException]

    val fallback = putStrLn("You didn't enter the number of seconds!") *> getAlarmDuration

    for {
      _        <- putStr("Please enter the number of seconds to sleep: ")
      input    <- getStrLn
      duration <- parseDuration(input) orElse fallback
    } yield duration
  }

  /**
    * EXERCISE 16
    *
    * Create a program that asks the user for a number of seconds to sleep,
    * sleeps the specified number of seconds using ZIO.sleep(d), concurrently
    * prints a dot every second that the alarm is sleeping for, and then
    * prints out a wakeup alarm message, like "Time to wakeup!!!".
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      duration  <- getAlarmDuration
      times     = (duration.toMillis / 1000).toInt - 1
      printDots = (putStr(".") *> clock.sleep(1.second)).repeat(Schedule.recurs(times))
      _         <- clock.sleep(duration) &> printDots
      _         <- putStrLn("Wake up!")
    } yield ())
      .foldM(e => putStrLn(e.getMessage) *> ZIO.succeed(1), _ => ZIO.succeed(0))
}

object ComputePi extends App {
  import zio.random._
  import zio.console._
  import java.lang.Runtime.getRuntime

  /**
    * Some state to keep track of all points inside a circle,
    * and total number of points.
    */
  final case class PiState(
      inside: Ref[Long],
      total: Ref[Long]
  )

  /**
    * A function to estimate pi.
    */
  def estimatePi(inside: Long, total: Long): Double =
    (inside.toDouble / total.toDouble) * 4.0

  /**
    * A helper function that determines if a point lies in
    * a circle of 1 radius.
    */
  def insideCircle(x: Double, y: Double): Boolean =
    Math.sqrt(x * x + y * y) <= 1.0

  /**
    * An effect that computes a random (x, y) point.
    */
  val randomPoint: ZIO[Random, Nothing, (Double, Double)] =
    nextDouble zip nextDouble

  /**
    * EXERCISE 17
    *
    * Build a multi-fiber program that estimates the value of `pi`. Print out
    * ongoing estimates continuously until the estimation is complete.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      sampleSize  <- readSampleSize(args)
      concurrency <- ZIO.effectTotal(getRuntime.availableProcessors)
      state       <- initState
      _           <- runSimulation(sampleSize, concurrency, state)
      _           <- printFinalEstimate(state, sampleSize)
    } yield ())
      .foldM(e => putStrLn(e.getMessage) as 1, _ => ZIO.succeed(0))

  private def readSampleSize(args: List[String]) =
    args.headOption.fold[IO[IllegalArgumentException, Int]](
      ZIO.fail(new IllegalArgumentException("No sample size given."))) { input: String =>
      ZIO.effect(input.toInt).refineToOrDie[NumberFormatException]
    }

  def initState: ZIO[Any, Nothing, PiState] =
    for {
      inside <- Ref.make(0L)
      total  <- Ref.make(0L)
    } yield PiState(inside, total)

  def runSimulation(sampleSize: Int,
                    concurrency: Int,
                    state: PiState): ZIO[Console with Random, Nothing, Unit] = {
    val remainderWorkload = workload(state, sampleSize % concurrency)
    val workloadCount     = sampleSize / concurrency
    val workloads         = List.fill(concurrency)(workload(state, workloadCount))
    ZIO.collectAllPar(remainderWorkload :: workloads).unit
  }

  private def workload(state: PiState, size: Int) =
    if (size != 0)
      (addSample(state) *> ongoingEstimate(state)).repeat(Schedule.recurs(size - 1)).unit
    else ZIO.unit

  private def addSample(state: PiState): ZIO[Random, Nothing, Unit] =
    for {
      point <- randomPoint
      _     <- state.inside.update(n => if (insideCircle(point._1, point._2)) n + 1 else n)
      _     <- state.total.update(_ + 1)
    } yield ()

  private def ongoingEstimate(state: PiState) =
    for {
      inside <- state.inside.get
      total  <- state.total.get
      _      <- putStrLn(s"Pi estimate: ${estimatePi(inside, total)}")
    } yield ()

  private def printFinalEstimate(state: PiState, sampleSize: Int) =
    for {
      finalInside <- state.inside.get
      finalTotal  <- state.total.get
      _ <- putStrLn(
            s"The final estimate of pi after $sampleSize samples is " +
              s"${estimatePi(finalInside, finalTotal)}.")
    } yield ()
}

object StmSwap extends App {
  import zio.console._
  import zio.stm._

  /**
    * EXERCISE 18
    *
    * Demonstrate the following code does not reliably swap two values in the
    * presence of concurrency.
    */
  def exampleRef = {
    def swap[A](ref1: Ref[A], ref2: Ref[A]): UIO[Unit] =
      for {
        v1 <- ref1.get
        v2 <- ref2.get
        _  <- ref2.set(v1)
        _  <- ref1.set(v2)
      } yield ()

    for {
      ref1   <- Ref.make(100)
      ref2   <- Ref.make(0)
      fiber1 <- swap(ref1, ref2).repeat(Schedule.recurs(100)).fork
      fiber2 <- swap(ref2, ref1).repeat(Schedule.recurs(100)).fork
      _      <- (fiber1 zip fiber2).join
      value  <- (ref1.get zipWith ref2.get)(_ + _)
    } yield value
  }

  /**
    * EXERCISE 19
    *
    * Using `STM`, implement a safe version of the swap function.
    */
  def exampleStm = {
    def swap[A](ref1: TRef[A], ref2: TRef[A]): UIO[Unit] =
      ???

    for {
      ref1   <- TRef.make(100).commit
      ref2   <- TRef.make(0).commit
      fiber1 <- swap(ref1, ref2).repeat(Schedule.recurs(100)).fork
      fiber2 <- swap(ref2, ref1).repeat(Schedule.recurs(100)).fork
      _      <- (fiber1 zip fiber2).join
      value  <- (ref1.get zipWith ref2.get)(_ + _).commit
    } yield value
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    exampleRef.map(_.toString).flatMap(putStrLn) as 0
}

object StmLock extends App {
  import zio.console._
  import zio.stm._

  /**
    * EXERCISE 20
    *
    * Using STM, implement a simple binary lock by implementing the creation,
    * acquisition, and release methods.
    */
  class Lock private (tref: TRef[Boolean]) {
    def acquire: UIO[Unit] = ???
    def release: UIO[Unit] = ???
  }
  object Lock {
    def make: UIO[Lock] = ???
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      lock <- Lock.make
      fiber1 <- lock.acquire
                 .bracket_(lock.release)(putStrLn("Bob  : I have the lock!"))
                 .repeat(Schedule.recurs(10))
                 .fork
      fiber2 <- lock.acquire
                 .bracket_(lock.release)(putStrLn("Sarah: I have the lock!"))
                 .repeat(Schedule.recurs(10))
                 .fork
      _ <- (fiber1 zip fiber2).join
    } yield 0) as 1
}

object StmLunchTime extends App {
  import zio.console._
  import zio.stm._

  /**
    * EXERCISE 21
    *
    * Using STM, implement the missing methods of Attendee.
    */
  final case class Attendee(state: TRef[Attendee.State]) {
    import Attendee.State._

    def isStarving: STM[Nothing, Boolean] = ???

    def feed: STM[Nothing, Unit] = ???
  }
  object Attendee {
    sealed trait State
    object State {
      case object Starving extends State
      case object Full     extends State
    }
  }

  /**
    * EXERCISE 22
    *
    * Using STM, implement the missing methods of Table.
    */
  final case class Table(seats: TArray[Boolean]) {
    def findEmptySeat: STM[Nothing, Option[Int]] =
      seats
        .fold[(Int, Option[Int])]((0, None)) {
          case ((index, z @ Some(_)), _) => (index + 1, z)
          case ((index, None), taken) =>
            (index + 1, if (taken) None else Some(index))
        }
        .map(_._2)

    def takeSeat(index: Int): STM[Nothing, Unit] = ???

    def vacateSeat(index: Int): STM[Nothing, Unit] = ???
  }

  /**
    * EXERCISE 23
    *
    * Using STM, implement a method that feeds a single attendee.
    */
  def feedAttendee(t: Table, a: Attendee): STM[Nothing, Unit] =
    for {
      index <- t.findEmptySeat.collect { case Some(index) => index }
      _     <- t.takeSeat(index) *> a.feed *> t.vacateSeat(index)
    } yield ()

  /**
    * EXERCISE 24
    *
    * Using STM, implement a method that feeds only the starving attendees.
    */
  def feedStarving(table: Table, list: List[Attendee]): UIO[Unit] =
    ???

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val Attendees = 100
    val TableSize = 5

    for {
      attendees <- ZIO.foreach(0 to Attendees)(
                    i =>
                      TRef
                        .make[Attendee.State](Attendee.State.Starving)
                        .map(Attendee(_))
                        .commit
                  )
      table <- TArray
                .fromIterable(List.fill(TableSize)(false))
                .map(Table(_))
                .commit
      _ <- feedStarving(table, attendees)
    } yield 0
  }
}

object StmPriorityQueue extends App {
  import zio.console._
  import zio.duration._
  import zio.stm._

  /**
    * EXERCISE 25
    *
    * Using STM, design a priority queue, where lower integers are assumed
    * to have higher priority than higher integers.
    */
  class PriorityQueue[A] private (
      minLevel: TRef[Int],
      map: TMap[Int, TQueue[A]]
  ) {
    def offer(a: A, priority: Int): STM[Nothing, Unit] = ???

    def take: STM[Nothing, A] = ???
  }
  object PriorityQueue {
    def make[A]: STM[Nothing, PriorityQueue[A]] = ???
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _     <- putStrLn("Enter any key to exit...")
      queue <- PriorityQueue.make[String].commit
      lowPriority = ZIO.foreach(0 to 100) { i =>
        ZIO.sleep(1.millis) *> queue
          .offer(s"Offer: ${i} with priority 3", 3)
          .commit
      }
      highPriority = ZIO.foreach(0 to 100) { i =>
        ZIO.sleep(2.millis) *> queue
          .offer(s"Offer: ${i} with priority 0", 0)
          .commit
      }
      _ <- ZIO.forkAll(List(lowPriority, highPriority)) *> queue.take.commit
            .flatMap(putStrLn(_))
            .forever
            .fork *>
            getStrLn
    } yield 0).fold(_ => 1, _ => 0)
}

object StmReentrantLock extends App {
  import zio.stm._

  private final case class WriteLock(
      writeCount: Int,
      readCount: Int,
      fiberId: FiberId
  )
  private final class ReadLock private (readers: Map[Fiber.Id, Int]) {
    def total: Int = readers.values.sum

    def noOtherHolder(fiberId: FiberId): Boolean =
      readers.size == 0 || (readers.size == 1 && readers.contains(fiberId))

    def readLocks(fiberId: FiberId): Int =
      readers.get(fiberId).fold(0)(identity)

    def adjust(fiberId: FiberId, adjust: Int): ReadLock = {
      val total = readLocks(fiberId)

      val newTotal = total + adjust

      new ReadLock(
        readers =
          if (newTotal == 0) readers - fiberId
          else readers.updated(fiberId, newTotal)
      )
    }
  }
  private object ReadLock {
    val empty: ReadLock = new ReadLock(Map())

    def apply(fiberId: Fiber.Id, count: Int): ReadLock =
      if (count <= 0) empty else new ReadLock(Map(fiberId -> count))
  }

  /**
    * EXERCISE 26
    *
    * Using STM, implement a reentrant read/write lock.
    */
  class ReentrantReadWriteLock(data: TRef[Either[ReadLock, WriteLock]]) {
    def writeLocks: UIO[Int] = data.get.map(_.fold(_ => 0, _.writeCount)).commit

    def writeLocked: UIO[Boolean] = writeLocks.map(_ > 0)

    def readLocks: UIO[Int] = data.get.map(_.fold(_.total, _.readCount)).commit

    def readLocked: UIO[Boolean] = readLocks.map(_ > 0)

    val read: Managed[Nothing, Int] = ???

    val write: Managed[Nothing, Int] = ???
  }
  object ReentrantReadWriteLock {
    def make: UIO[ReentrantReadWriteLock] =
      TRef
        .make[Either[ReadLock, WriteLock]](Left(ReadLock.empty))
        .map(tref => new ReentrantReadWriteLock(tref))
        .commit
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = ???
}

object Sharding extends App {

  /**
    * EXERCISE 27
    *
    * Create N workers reading from a Queue, if one of them fails, then wait
    * for the other ones to process their current item, but terminate all the
    * workers.
    */
  def shard[R, E, A](
      queue: Queue[A],
      n: Int,
      worker: A => ZIO[R, E, Unit]
  ): ZIO[R, E, Nothing] =
    ???

  def run(args: List[String]) = ???
}

object Hangman extends App {

  import java.io.IOException

  import zio.console._
  import zio.random._

  /**
    * EXERCISE 28
    *
    * Implement an effect that gets a single, lower-case character from
    * the user.
    */
  lazy val getChoice: ZIO[Console, IOException, Char] =
    for {
      line <- getStrLn
      choice <- if (line.length == 1 && line.charAt(0).isLetter)
                 ZIO.succeed(line.charAt(0).toLower)
               else
                 putStr("Must enter a single letter: ") *> getChoice
    } yield choice

  /**
    * EXERCISE 29
    *
    * Implement an effect that prompts the user for their name, and
    * returns it.
    */
  lazy val getName: ZIO[Console, IOException, String] =
    for {
      _    <- putStr("Please enter your name: ")
      name <- getStrLn
      _ <- if (name.isEmpty) putStrLn("Please enter something!") *> getName
          else ZIO.unit
    } yield name

  /**
    * EXERCISE 30
    *
    * Implement an effect that chooses a random word from the dictionary.
    */
  lazy val chooseWord: ZIO[Random, Nothing, String] =
    nextInt(Dictionary.Dictionary.size).map(Dictionary.Dictionary(_))

  /**
    * EXERCISE 31
    *
    * Implement the main game loop, which gets choices from the user until
    * the game is won or lost.
    */
  def gameLoop(ref: Ref[State]): ZIO[Console, IOException, Unit] =
    for {
      oldState <- ref.get
      _        <- renderState(oldState)
      _        <- putStr("Guess the next letter: ")
      char     <- getChoice
      _        <- ref.update(_.addChar(char))
      newState <- ref.get
      _        <- renderState(newState)
      result   = guessResult(oldState, newState, char)
      _        <- handleGuessResult(ref, oldState, char, result)
    } yield ()

  private def handleGuessResult(ref: Ref[State],
                                oldState: State,
                                char: Char,
                                result: GuessResult) = {
    import GuessResult._
    result match {
      case Won =>
        putStrLn(
          s"Well done, ${oldState.name}! " +
            s"You guessed ${"\"" + oldState.word + "\""}.")
      case Lost =>
        putStrLn(
          s"Game over, ${oldState.name}! The word was " +
            s"${"\"" + oldState.word + "\""}. Try again next time.")
      case Correct =>
        putStrLn(s"You guessed a correct letter, '$char'.") *>
          gameLoop(ref)
      case Incorrect =>
        putStrLn(s"'$char' is not in the word.") *> gameLoop(ref)
      case Unchanged =>
        putStrLn(s"You already guessed '$char'.") *> gameLoop(ref)
    }
  }

  def renderState(state: State): ZIO[Console, Nothing, Unit] = {

    /**
      *
      * f     n  c  t  o
      *  -  -  -  -  -  -  -
      *
      * Guesses: a, z, y, x
      *
      */
    val word =
      state.word.toList
        .map(c => if (state.guesses.contains(c)) s" $c " else "   ")
        .mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

    val guesses = " Guesses: " + state.guesses.mkString(", ")

    val text = word + "\n" + line + "\n\n" + guesses + "\n"

    putStrLn(text)
  }

  final case class State(name: String, guesses: Set[Char], word: String) {
    def failures: Int = (guesses -- word.toSet).size

    def playerLost: Boolean = failures > 10

    def playerWon: Boolean = (word.toSet -- guesses).isEmpty

    def addChar(char: Char): State = copy(guesses = guesses + char)
  }

  sealed trait GuessResult

  object GuessResult {

    case object Won extends GuessResult

    case object Lost extends GuessResult

    case object Correct extends GuessResult

    case object Incorrect extends GuessResult

    case object Unchanged extends GuessResult

  }

  def guessResult(oldState: State, newState: State, char: Char): GuessResult =
    if (oldState.guesses.contains(char)) GuessResult.Unchanged
    else if (newState.playerWon) GuessResult.Won
    else if (newState.playerLost) GuessResult.Lost
    else if (oldState.word.contains(char)) GuessResult.Correct
    else GuessResult.Incorrect

  /**
    * EXERCISE 32
    *
    * Implement hangman using `Dictionary.Dictionary` for the words,
    * and the above helper functions.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _     <- putStrLn("Welcome to Hangman!")
      name  <- getName
      word  <- chooseWord
      state <- Ref.make(State(name, Set.empty, word))
      _     <- gameLoop(state)
    } yield ())
      .foldM(e => putStrLn(e.getMessage) *> ZIO.succeed(1), _ => ZIO.succeed(0))
}

/**
  * GRADUATION PROJECT
  *
  * Implement a game of tic tac toe using ZIO, then develop unit tests to
  * demonstrate its correctness and testability.
  */
object TicTacToe extends App {

  import java.io.IOException

  import zio.console._

  sealed trait Mark {
    final def renderChar: Char = this match {
      case Mark.X => 'X'
      case Mark.O => 'O'
    }

    final def render: String = renderChar.toString
  }

  object Mark {

    case object X extends Mark

    case object O extends Mark

  }

  final case class Board private (value: Vector[Vector[Option[Mark]]]) {

    /**
      * Retrieves the mark at the specified row/col.
      */
    final def get(row: Int, col: Int): Option[Mark] =
      value.lift(row).flatMap(_.lift(col)).flatten

    /**
      * Places a mark on the board at the specified row/col.
      */
    final def place(row: Int, col: Int, mark: Mark): Option[Board] =
      if (row >= 0 && col >= 0 && row < 3 && col < 3 && value(row)(col).isEmpty)
        Some(
          copy(value = value.updated(row, value(row).updated(col, Some(mark))))
        )
      else None

    /**
      * Renders the board to a string.
      */
    def render: String =
      value
        .map(_.map(_.fold(" ")(_.render)).mkString(" ", " | ", " "))
        .mkString("\n---|---|---\n")

    /**
      * Returns which mark won the game, if any.
      */
    final def won: Option[Mark] =
      if (wonBy(Mark.X)) Some(Mark.X)
      else if (wonBy(Mark.O)) Some(Mark.O)
      else None

    def gameIsOver: Boolean =
      won.isDefined || value.forall(_.forall(_.isDefined))

    private final def wonBy(mark: Mark): Boolean =
      wonBy(0, 0, 1, 1, mark) ||
        wonBy(0, 2, 1, -1, mark) ||
        wonBy(0, 0, 0, 1, mark) ||
        wonBy(1, 0, 0, 1, mark) ||
        wonBy(2, 0, 0, 1, mark) ||
        wonBy(0, 0, 1, 0, mark) ||
        wonBy(0, 1, 1, 0, mark) ||
        wonBy(0, 2, 1, 0, mark)

    private final def wonBy(
        row0: Int,
        col0: Int,
        rowInc: Int,
        colInc: Int,
        mark: Mark
    ): Boolean =
      extractLine(row0, col0, rowInc, colInc).collect { case Some(v) => v }.toList == List
        .fill(3)(mark)

    private final def extractLine(
        row0: Int,
        col0: Int,
        rowInc: Int,
        colInc: Int
    ): Iterable[Option[Mark]] =
      for {
        row <- (row0 to (row0 + rowInc * 2))
        col <- (col0 to (col0 + colInc * 2))
      } yield value(row)(col)
  }

  object Board {
    final val empty = new Board(Vector.fill(3)(Vector.fill(3)(None)))

    def fromChars(
        first: Iterable[Char],
        second: Iterable[Char],
        third: Iterable[Char]
    ): Option[Board] =
      if (first.size != 3 || second.size != 3 || third.size != 3) None
      else {
        def toMark(char: Char): Option[Mark] =
          if (char.toLower == 'x') Some(Mark.X)
          else if (char.toLower == 'o') Some(Mark.O)
          else None

        Some(
          new Board(
            Vector(
              first.map(toMark).toVector,
              second.map(toMark).toVector,
              third.map(toMark).toVector
            )
          )
        )
      }
  }

  val TestBoard = Board
    .fromChars(
      List(' ', 'O', 'X'),
      List('O', 'X', 'O'),
      List('X', ' ', ' ')
    )
    .get
    .render

  import Hangman.getName

  case class State private (xName: String, oName: String, board: Board, turn: Mark) {
    def takeTurn(row: Int, col: Int): MoveResult =
      board.place(row, col, turn).map { newBoard =>
        State(xName, oName, newBoard, if (turn == Mark.X) Mark.O else Mark.X)
      } match {
        case Some(newState) =>
          if (this == newState) BoardUnchanged(newState)
          else if (newState.board.gameIsOver) {
            newState.board.won
              .map(winner => Won(newState, winner))
              .getOrElse(Draw(newState))
          } else BoardChanged(newState)
        case None => IncorrectInput(this)
      }

    def nameOf(mark: Mark): String = mark match {
      case Mark.X => xName
      case Mark.O => oName
    }
  }

  object State {
    def init(xName: String, yName: String) =
      State(xName, yName, Board.empty, Mark.X)
  }

  sealed abstract class MoveResult(state: State)
  sealed abstract class GameOver(state: State)     extends MoveResult(state)
  final case class Won(state: State, winner: Mark) extends GameOver(state)
  final case class Draw(state: State)              extends GameOver(state)
  final case class BoardChanged(state: State)      extends MoveResult(state)
  final case class BoardUnchanged(state: State)    extends MoveResult(state)
  final case class IncorrectInput(state: State)    extends MoveResult(state)

  /**
    * The entry point to the game will be here.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _          <- putStrLn("Welcome to Tic Tac Toe")
      _          <- putStrLn("Please enter the first player's name.")
      firstName  <- getName
      _          <- putStrLn("Please enter the second player's name.")
      secondName <- getName
      refState   <- Ref.make(State(firstName, secondName, Board.empty, Mark.X))
      _          <- gameLoop(refState)
    } yield ())
      .foldM(e => putStrLn(e.getMessage) *> ZIO.succeed(1), _ => ZIO.succeed(0))

  def gameLoop(ref: Ref[State]): ZIO[Console, IOException, Unit] =
    for {
      oldState   <- ref.get
      _          <- putStrLn(oldState.board.render)
      rowCol     <- readPlacement(oldState)
      (row, col) = rowCol
      _ <- oldState.takeTurn(row, col) match {
            case Draw(state) =>
              ref.update(_ => state) *> putStrLn(
                "Game over. The game ended " +
                  "in a draw.")
            case Won(state, mark) =>
              ref.update(_ => state) *> putStrLn(s"Game over. ${state.nameOf(mark)} won!")
            case BoardChanged(state) =>
              ref.update(_ => state) *> gameLoop(ref)
            case BoardUnchanged(_) =>
              putStrLn(
                "You tried to place a mark in an occupied slot. Try " +
                  "putting a mark some place else") *> gameLoop(ref)
            case IncorrectInput(_) =>
              putStrLn("You entered an invalid spot. Try again") *> gameLoop(ref)
          }
    } yield ()

  def readPlacement(oldState: State): ZIO[Console, IOException, (Int, Int)] =
    for {
      _ <- putStr(
            s"Enter where to place the next ${oldState.turn.render} " +
              "(ex. \"1 1\" without quotes ): ")
      line   <- getStrLn
      tokens = line.split(" ")
      valid  = tokens.size == 2 && tokens.forall(_.matches("\\d+"))
      numbers <- if (valid) {
                  ZIO
                    .effect(tokens.map(_.toInt))
                    .orDie
                    .map(arr => (arr(0), arr(1)))
                } else
                  readPlacement(oldState)
    } yield numbers
}
