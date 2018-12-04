package problems

import scala.io.Source

/**
  * Created by jon on 12/4/18.
  */
object p1a extends App {
  val lines = Source.fromFile("./data/p1").getLines()
  val integers = lines.map(_.toInt)
  println(integers.sum)
}

object p1b extends App {
  val lines = Source.fromFile("./data/p1").getLines()
  val integers = lines.map(_.toInt).toList
  val stream = Stream.continually(integers.toStream).flatten

  def findFirstRepeat(sum: Int = 0, seen: Set[Int] = Set.empty, it: Stream[Int] = stream, iterations: Int = 0): Int =
    if (seen.contains(sum)) sum else findFirstRepeat(sum + it.head, seen + sum, it.tail, iterations + 1)

  println(findFirstRepeat())
}
