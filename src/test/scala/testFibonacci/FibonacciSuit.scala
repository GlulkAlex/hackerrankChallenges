package testFibonacci

import org.scalatest.FunSuite

import common.FilesIO._
import common.ShowProgress._
import fibonacciFindingEasy.Solution.fibFrom
import fibonacciFindingEasy.Solution.fibFrom2
import fibonacciFindingEasy.Solution._

import java.text.SimpleDateFormat
import java.util.Calendar
import java.io._
import java.io.PrintWriter
import java.io.FileReader
import java.io.FileNotFoundException
import java.io.IOException

import scala.io._
import scala.io.Source
import scala.collection.mutable.Buffer
import scala.collection.BitSet
import scala.util.matching.Regex
import scala.util.{Try, Success, Failure}

/**
 * Created by gluk-alex on 8/28/15.
 */
class FibonacciSuit
  extends FunSuite {

  case class FibInput(a: Int, b: Int, n: Int)

  test(
        "1: 'fibFrom2'" +
          "should " +
          "return right sequence member"
      ) {
          val outPutFilePath: String =
            "/home/gluk-alex/Documents/"
          val outPutFileName: String =
            "graphPostOrder.txt"
          val inPutFilePath: String =
            "/home/gluk-alex/Documents/sbt_projects/" +
              "hackerrankChallenges/" +
              "src/test/scala/" +
              "testFibonacci/"
          val inPutFileName: String =
            "SampleInput.txt"
          val inPutFileName2: String =
            "SampleOutput.txt"
          val expectedFileContentIter: Iterator[String] =
          //Iterator.empty
            readFromFile(
                          fileName =
                            inPutFileName2,
                          filePath =
                            inPutFilePath
                        )
          /*reduce / control input size*/
          //.take(inputTakeNumber)
          //.duplicate
          /*lazy val (actualFileContentIter, fileContentIter):
          (Iterator[String], Iterator[String]) =*/
          val inputFileContentIter: Iterator[String] =
          //Iterator.empty
            readFromFile(
                          fileName =
                            inPutFileName,
                          filePath =
                            inPutFilePath
                        )
          val linesInInput: Int =
            inputFileContentIter.next().toInt
          val expectedResults: List[Long] =
            expectedFileContentIter
            .map(_.toLong)
            .toList
          /*lazy*/ val startDateTime1:
          java.util.Date =
            Calendar.getInstance().getTime()
          /*lazy*/ val startTimeStamp1: Long =
            System.currentTimeMillis
          /*Date and Time Pattern */
          val timeStampFormat: SimpleDateFormat =
            new SimpleDateFormat("HH:mm:ss.SSS")
          lazy val startStampString1 =
            timeStampFormat.format(startDateTime1)
          println(s"'Test' started at:" +
                    startStampString1)

          val inputParams: List[FibInput] =
            (for {
              line <- inputFileContentIter
            } yield {
                /*
                n-bounds:
                longNumber: Long = 1000000000
                doubleNumber: Double = 1.0E9
                intNumber: Int = 1000000000
                 */
                val Array(a, b, n): Array[Int] =
                  line
                  .split(" ")
                  .map(_.toInt)

                FibInput(a: Int, b: Int, n: Int)
              }).toList
          val takeNumber: Int = 10
          println(
                   s"First:" + takeNumber +
                     s" parameters from input file are:\n" +
                     inputParams
                     .map { case FibInput(a: Int, b: Int, n: Int) =>
                       s"{a:$a,b:$b,n:$n}"
                          }
                     .take(takeNumber)
                 )
          val results: List[Long] =
          //(
            for {
              FibInput(a: Int, b: Int, n: Int) <- inputParams
                                                  //.take(2)
              //.drop(4)
                                                  //.take(1)
              //.apply(3)
              //.head
            } yield {
              /*work, but wrong*/
              /*fibFrom(a = a.toInt, b = b.toInt)
              .take(n.toInt)
              .head*/
              /*work, but slow*/
              //fibFrom2(a = a, b = b, elemOrder = n)
              /*work, but wrong math*/
              fibonacciDoublingFromRecursive2(a = a, b = b, elemOrder = n)
              .toLong
              /*work, but wrong*/
              /*fastFibonacciDoubling(
                                     initA=a,
                                     initB=b,
                                     n=n
                                   )
              .longValue()*/
            }
          //)
          //.toList
          lazy val endDateTime1 = Calendar.getInstance().getTime()
          lazy val endTimeStamp1: Long = System.currentTimeMillis()
          lazy val endStampString1 = timeStampFormat.format(endDateTime1)
          println(s"Done at:" + endStampString1)
          println(s"time difference is:" +
                    (endTimeStamp1 - startTimeStamp1) +
                    " Millis or:" +
                    convertLongToTimeString(
                                             timeNumberMillis =
                                               endTimeStamp1 -
                                                 startTimeStamp1,
                                             colored = false
                                           )
                 )

          assume(
                  //results.length == expectedResults.length &&
                    results == expectedResults,
                  s"must be equal")
        }
  ignore(
          "2: ''" +
            "should " +
            " "
        ) {
            val linesInInput: Int =
              scala.io.StdIn.readInt()
            val inputParams: List[FibInput] =
              (for {
                index <- 1 to linesInInput
              } yield {
                  val Array(a, b, n): Array[Int] =
                    scala.io.StdIn.readLine()
                    .split(" ")
                    .map(_.toInt)

                  FibInput(a: Int, b: Int, n: Int)
                }).toList
            /*side effect*/
            inputParams.foreach {
                                  case FibInput(a: Int, b: Int, n: Int) =>
                                    //StdOut
                                    scala.Console
                                    .println(
                                        fibFrom2(
                                                  a = a,
                                                  b = b,
                                                  elemOrder = n))
                                }

            assume(
                    inputParams.nonEmpty,
                    s"must be 'nonEmpty'")
          }

}
