package problems

import scala.io.Source

/**
  * Created by jon on 12/8/18.
  */
object p3a extends App{

  case class Rectangle(id: Int, startX: Int, startY: Int, width: Int, height: Int)


  val lines = Source.fromFile("./data/p3").getLines()

  val parseLine = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r
  val rectangles = lines.map({
    case parseLine(id, x, y, w, h) => Rectangle(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
  })

  type CellSet = Set[(Int, Int)]

  def generateCellSet(r: Rectangle): CellSet = (for {
    x <- r.startX until r.startX + r.width
    y <- r.startY until r.startY + r.height
  } yield (x, y)).toSet

  val takenInitial : CellSet = Set.empty
  val overlapInitial : CellSet = Set.empty
  val (taken, overlap) = rectangles.foldLeft(takenInitial, overlapInitial)((t: (CellSet, CellSet), r: Rectangle) => {
    val newCells = generateCellSet(r)
    (t._1 ++ newCells, t._2 ++ t._1.intersect(newCells))
  })
  println(overlap.size)

}

object p3b extends App{

  case class Rectangle(id: Int, startX: Int, startY: Int, width: Int, height: Int)


  val lines = Source.fromFile("./data/p3").getLines()

  val parseLine = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r
  val rectangles = lines.map({
    case parseLine(id, x, y, w, h) => Rectangle(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
  })

  case class Cell(x: Int, y: Int)
  case class MarkedCell(id: Int, x: Int, y: Int)

  def generateMarkedCellSet(r: Rectangle): Seq[MarkedCell] = for {
    x <- r.startX until r.startX + r.width
    y <- r.startY until r.startY + r.height
  } yield MarkedCell(r.id, x, y)

  val takenInitial : Seq[MarkedCell] = Seq.empty
  val overlapInitial : Set[Cell] = Set.empty
  val (taken, overlap) = rectangles.foldLeft(takenInitial, overlapInitial)((t: (Seq[MarkedCell], Set[Cell]), r: Rectangle) => {
    val newCells = generateMarkedCellSet(r)
    (t._1 ++ newCells, t._2 ++ t._1.map(mc => Cell(mc.x, mc.y)).toSet.intersect(newCells.map(mc => Cell(mc.x, mc.y)).toSet))
  })
  val ans = taken.groupBy(_.id).find(mcg => mcg._2.map(mc => Cell(mc.x, mc.y)).toSet.intersect(overlap).isEmpty).getOrElse((-1, List(MarkedCell(-1, -1, -1))))._1
  println(ans)
}
