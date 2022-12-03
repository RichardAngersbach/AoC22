package day3

import scala.io.Source

object Main {
  def main(args : Array[String]) : Unit = {
    val path = getClass.getResource("").getPath
    val inputFile = s"${ path }input.txt"
    val lines = Source.fromFile(inputFile).getLines.toSeq
    val compartments = lines.map(line => line.splitAt(line.size / 2))
    val groups = lines.grouped(3).toSeq

    def priority(item : Char) = if (item.isLower) item - 'a' + 1 else item - 'A' + 27
    def findCommonItems(s1 : String, s2: String) = s1.toCharArray.filter(s2.contains(_)).toSet

    // part one
    println(compartments.map(tup => findCommonItems(tup._1, tup._2)).map(common => common.map(priority).sum).sum)

    // part two
    def findBadge(s0 : String, s1 : String, s2 : String) = s0.intersect(s1).intersect(s2).head

    println(groups.map(group => priority(findBadge(group(0), group(1), group(2)))).sum)
  }
}