package problems

import common.BaseProblem

object Problem03 extends BaseProblem(3) {

  case class Rectangle(id: Int, startX: Int, startY: Int, width: Int, height: Int)
  case class Cell(x: Int, y: Int)
  case class MarkedCell(id: Int, x: Int, y: Int)

  val lines = readInput().toList

  val parseLine = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r
  val rectangles = lines.map({
    case parseLine(id, x, y, w, h) => Rectangle(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
  })

  def generateCellSet(r: Rectangle): Set[Cell] = (for {
    x <- r.startX until r.startX + r.width
    y <- r.startY until r.startY + r.height
  } yield Cell(x, y)).toSet

  def generateMarkedCellSet(r: Rectangle): Seq[MarkedCell] = for {
    x <- r.startX until r.startX + r.width
    y <- r.startY until r.startY + r.height
  } yield MarkedCell(r.id, x, y)

  val takenInitial : Set[Cell] = Set.empty
  val overlapInitial : Set[Cell] = Set.empty
  val (taken, overlap) = rectangles.foldLeft(takenInitial, overlapInitial)((t: (Set[Cell], Set[Cell]), r: Rectangle) => {
    val newCells = generateCellSet(r)
    (t._1 ++ newCells, t._2 ++ t._1.intersect(newCells))
  })

  def solutionA = overlap.size

  val takenInitialSeq : Seq[MarkedCell] = Seq.empty
  val (taken2, overlap2) = rectangles.foldLeft(takenInitialSeq, overlapInitial)((t: (Seq[MarkedCell], Set[Cell]), r: Rectangle) => {
    val newCells = generateMarkedCellSet(r)
    (t._1 ++ newCells, t._2 ++ t._1.map(mc => Cell(mc.x, mc.y)).toSet.intersect(newCells.map(mc => Cell(mc.x, mc.y)).toSet))
  })

  // TODO: faster. use groupBy
  def solutionB =
    taken2.groupBy(_.id).find(mcg => mcg._2.map(mc => Cell(mc.x, mc.y)).toSet.intersect(overlap).isEmpty).getOrElse((-1, List(MarkedCell(-1, -1, -1))))._1

  printBoth
}
