
import scala.collection.mutable.PriorityQueue
import scala.math.Ordering.Implicits._

class Node(val id: String, val value: Int, val left: Node, val right: Node) {
  override def toString = s"Node[id}:$id|value:$value|left:$left|right:$right]"
}

class Leaf(val leaf_id: String, value: Int) extends Node(leaf_id, value, null, null) {
  override def toString = s"Leaf[$id}|value=$value]"

}

object Ord extends Ordering[Node] {
  def compare(node1: Node, node2: Node) = {
    if (node1.value == node2.value) {
      node1.id.compare(node2.id)
    } else {
      node1.value.compare(node2.value)
    }
  }
}

object Huffman {

    def generatePriorityQueue(frequencies: Map[Char, Int]): PriorityQueue[Node] = {
    val pq = PriorityQueue.empty[Node](Ord.reverse)
      //Ordering.by((_: Node).value).reverse)
    for (item <- frequencies.toList)
      pq.enqueue(new Leaf(item._1.toString, item._2))
    pq

  }

  def generateTree(priorityQueue: PriorityQueue[Node]): Node = {
    if (priorityQueue.size > 1) {
      val smallest = priorityQueue.dequeue()
      val largest = priorityQueue.dequeue()
      val newNode = new Node(smallest.id + largest.id,
        smallest.value + largest.value, smallest, largest)
      priorityQueue.enqueue(newNode)
      generateTree(priorityQueue)
    } else {
      priorityQueue.dequeue()
    }
  }

  def generateFrequencies(inputText: String): Map[Char, Int] = {
    // 1. generate fre
    inputText.foldLeft(Map[Char, Int]())(
      (acc, item) => {
        val count = acc.getOrElse(item, 0) + 1
        acc + (item -> count)
      })
  }

  def getNode(id:String, huffmanTree:Node, accumulator:String):String = huffmanTree match {
    case l:Leaf => if (l.id.equals(id)) accumulator else ""
    case n:Node => 
          { 
            val res = getNode(id, huffmanTree.left, accumulator + "0")
           if (res.equals("")) getNode(id, huffmanTree.right, accumulator + "1")
           else res
          }
    
  }
  
  
  def generateHuffmanCodes(inputText: String): Unit = {

    import scala.collection.mutable.PriorityQueue
    val res = generateFrequencies(inputText.replace(" ", ""))
    val pq = generatePriorityQueue(res)
    val node = generateTree(pq)
    for (item <- res.keys.toSeq.sorted) {
      print(item  + ": " + getNode(item.toString, node, "") + "; ")
    }
  }


}