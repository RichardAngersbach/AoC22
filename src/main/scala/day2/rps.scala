package day2

import scala.io.Source

object Main {

  def main(args : Array[String]) : Unit = {
    val path = getClass.getResource("").getPath
    val lines = Source.fromFile(s"${ path }input.txt").getLines.toSeq.mkString.grouped(3).map(s => s.charAt(0) -> s.charAt(2)).toSeq


    def getWinIndex(choice : Int) = (((choice - 1) % 3) + 3) % 3
    def getWinIndexInv(choice : Int) = (choice + 1) % 3

    // part one
    def calcScore(l : Char, r : Char) = (r - 'X', l - 'A') match {
      case (right : Int, left : Int) if getWinIndex(right) == left => right + 1 + 6 // win
      case (right : Int, left : Int) if right == left              => right + 1 + 3 // draw
      case (right : Int, _)                                        => right + 1 // loss
    }

    println(lines.map(m => calcScore(m._1, m._2)).sum)

    // part two
    def ultraTopSecretStrategyGuide(l : Char, r : Char) = {
      def choice : Int = ((r - 'X'), (l - 'A')) match {
        case (0, left) => getWinIndex(left) // lose
        case (1, left) => left // draw
        case (2, left) => getWinIndexInv(left)// win
      }

      calcScore(l, ('X' + choice).toChar)
    }

    println(lines.map(m => ultraTopSecretStrategyGuide(m._1, m._2)).sum)
  }
}
