# CodeEval
Scala Exercises for CodeEval and Project Euler

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

object CodeEvalChallenges {

  def pascalTriangle(row: Int, col: Int): Int = {
    if (row == col) 1
    else if (row < 0 || col < 0 || col > row) 0
    else pascalTriangle(row - 1, col - 1) + pascalTriangle(row - 1, col)
  }

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
      println(s"New node $newNode created")
      priorityQueue.enqueue(newNode)
      generateTree(priorityQueue)
    } else {
      priorityQueue.dequeue()
    }
  }

  def generateFrequencies2(inputText: String): Map[Char, Int] = {
    // 1. generate fre
    inputText.foldLeft(Map[Char, Int]())(
      (acc, item) => {
        val count = acc.getOrElse(item, 0) + 1
        acc + (item -> count)
      })
  }

  def getHuffmanCodeFor(item:String, huffmanTree:Node):String = {
    null
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
  
  
  def generateHuffmanCodes(inputText: String): Node = {

    /**
     * Use cases
     * abc
     * ilovecodeeval
     *
     * output
     *
     * a: 10; b: 11; c: 0;
     * a: 1000; c: 1001; d: 1010; e: 01; i: 1011; l: 110; o: 111; v: 00;
     *
     *
     */
    // generate frequencies
    val frequencies = generateFrequencies2(inputText)

    // generate priority queue
    val pq = generatePriorityQueue(frequencies)

    val tree = generateTree(pq)

    null
  }

}


### Tests


import org.junit._
import scala.io._
import org.junit.Assert._

import CodeEvalChallenges._
import org.junit._

class CodeEvalTestSuite  { 

  
  @Test @Ignore def testPascalTriangle()  {
    assertEquals(1, pascalTriangle(0,0))
    assertEquals(10, pascalTriangle(5,2))
    assertEquals(3, pascalTriangle(3,1))
    
    assertFalse(true)
  }
  @Test
  def generateFrequencies(){
    import scala.collection.mutable.PriorityQueue
    val res = generateFrequencies2("ilovecodeeval") 
    println(res)
    val pq = generatePriorityQueue(res)
    val node = generateTree(pq)
    println(node)
    println("finding hfufman now..")
    for (item <- res.keys) {
      println(item + "==" + getNode(item.toString, node, ""))
    }
    println(getNode("a", node, ""))
    
    """
    def deq(pq:PriorityQueue[Node]):Unit = {
      if (pq.isEmpty) {
        println("Outta here")
        return
      }else {
        val item = pq.dequeue()
        println(s"Getting out:$item")
        deq(pq)
      }
    }
    deq(pq)
    """
    
  }
  
}



