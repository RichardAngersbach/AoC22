package day4

import scala.io.Source

object Main {
  def main(args : Array[String]) : Unit = {
    val path = getClass.getResource("").getPath
    val inputFile = s"${ path }input.txt"
    val lines = Source.fromFile(inputFile).getLines.toSeq

    def partStringSeqByChar(seq : Seq[String], char: Char) = seq.map(_.split(char)).map(a => (a.head, a.reverse.head))

    val splitLines = partStringSeqByChar(lines, ',')
    val left = splitLines.map(_._1)
    val right = splitLines.map(_._2)

    def strTupToIntTup(tup : (String, String)) = (tup._1.toInt, tup._2.toInt)

    val boundariesLeft = partStringSeqByChar(left, '-').map(strTupToIntTup)
    val boundariesRight = partStringSeqByChar(right, '-').map(strTupToIntTup)

    // part one
    def fullOverlap(minL : Int, maxL : Int, minR : Int, maxR : Int) = (minL >= minR && maxL <= maxR) || (minL <= minR && maxL >= maxR)
    val c1 = boundariesLeft.zipWithIndex.count { case (tup, i) => fullOverlap(tup._1, tup._2, boundariesRight(i)._1, boundariesRight(i)._2) }
    println(c1)

    // part two (naive)
    def partialOverlap(minL : Int, maxL : Int, minR : Int, maxR : Int) = Range.inclusive(minL, maxL).intersect(Range.inclusive(minR, maxR)).nonEmpty
    val c2 = boundariesLeft.zipWithIndex.count { case (tup, i) => partialOverlap(tup._1, tup._2, boundariesRight(i)._1, boundariesRight(i)._2) }
    println(c2)
  }
}
