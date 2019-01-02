package problems

import common.BaseProblem

object Problem08 extends BaseProblem(8) {

    case class Node(children: Seq[Node], metadata: Seq[Int]) {
      def metadataSum: Int = metadata.sum + children.map(_.metadataSum).sum
      def value: Int =
        children match {
          case _ :: _ => metadata.map(m => if (0 < m && m < children.length + 1) children(m - 1).value else 0).sum
          case Nil => metadataSum
        }
    }

    def buildTree(intSeq: List[Int]) : (List[Int], Node) = {
      val numChildren :: numMetadata :: content = intSeq
      val (rem, children) = (0 until numChildren).foldLeft(content, Seq.empty[Node])(
        (acc, _) => {
          val x = buildTree(acc._1)
          (x._1, acc._2 :+ x._2)
        }
      )
      val (metadata, extra) = rem.splitAt(numMetadata)
      (extra, Node(children, metadata))
    }

  private val tree = buildTree(readInput().next().split(' ').map(_.toInt).toList)._2
  def solutionA = tree.metadataSum
  def solutionB = tree.value
}