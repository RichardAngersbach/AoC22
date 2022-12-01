package day1

import scala.io.Source

object Main {

  def repeatedSpan[T](iterable: Iterable[T], pred: T => Boolean): List[Iterable[T]] = {
    if (iterable.isEmpty) Nil
    else {
      val (matches, rest) = iterable.span(pred)
      if (matches.isEmpty) rest.take(1) :: repeatedSpan(rest.tail, pred)
      else matches :: repeatedSpan(rest, pred)
    }
  }

  def printResult(calories : Int, id : Int) = println( s"Found elf #$id with $calories calories" )

  def isWhitespace(s : String) = s.trim.nonEmpty

  def main(args: Array[String]): Unit = {
    val path = getClass.getResource("").getPath
    val lines = Source.fromFile(s"${ path }input.txt").getLines.toSeq
    val splitupLines = repeatedSpan(lines, isWhitespace).map(_.filter(isWhitespace)).map(_.map(_.toInt).sum)
    val sortedCalories = splitupLines.zipWithIndex.sortBy(_._1)(Ordering[Int].reverse)

    // part one
    printResult(sortedCalories.head._1, sortedCalories.head._2)

    // part two
    println("Total calories part two: " + sortedCalories.take(3).map(_._1).sum)
  }
}
