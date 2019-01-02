package problems

import common.BaseProblem

object Problem08 extends BaseProblem(8) {

    private val full : Seq[Int] = readInput().next().split(' ').map(_.toInt)

    case class Node(children: Seq[Node], metadata: Seq[Int]) {
      def metadataSum: Int = metadata.sum + children.map(_.metadataSum).sum
      def value: Int =
        if (children.isEmpty)
          metadataSum
        else
          metadata.map(m => if (0 < m && m < children.length + 1) children(m - 1).value else 0).sum
    }

    def buildTree(intSeq: Seq[Int]) : (Seq[Int], Node) = {
      val (numChildren, numMetadata, content) = (intSeq.head, intSeq(1), intSeq.drop(2))
      val (rem, children) = (0 until numChildren).foldLeft(content, Seq.empty[Node])(
        (t, _) => {
          val x = buildTree(t._1)
          (x._1, t._2 :+ x._2)
        }
      )
      val (metadata, extra) = rem.splitAt(numMetadata)
      (extra, Node(children, metadata))
    }

  val tree = buildTree(full)._2
  def solutionA = tree.metadataSum
  def solutionB = tree.value

//  def solutionA = buildTree(fullIntList).metadataSum
//  def solutionB = ""
}
