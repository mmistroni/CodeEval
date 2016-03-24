import org.junit._
import Assert._

import Huffman._

class HuffmanTestSuite {

  @Test 
  def testGenerateFrequencies() {
    import scala.collection.mutable.PriorityQueue
    val res = generateFrequencies("ilovecodeeval")
    println(res)
    val pq = generatePriorityQueue(res)
    val node = generateTree(pq)
    println(node)
    println("finding hfufman now..")
    assertEquals("1000", getNode("a", node, ""))
    assertEquals("00", getNode("v", node, ""))
    assertEquals("01", getNode("e", node, ""))
  }  
    
  
}