import org.junit._
import Assert._

import PokerHands._
import Stack._
import ReverseAndAdd._

class StackTestSuite {

  @Test @Ignore 
  def testCodeEvalUseCases() {
    
    val usecases = List(
        "1 2 3 4",
        "10 -2 3 4")
        
    val expectedResults = List(
        List("4","2"),
        List("4","-2"))
    
    
    for (idx <- 0 until usecases.size) {
      val lst = usecases(idx).split(" ").toList
      val expected = expectedResults(idx)
      assertEquals(expected, createStack(lst))
      
    }
  }
  
  @Test
  def reveseAndAdd() {
    val res = reverseAndAdd(195)
    assertEquals((4, 9339), res)
  }
  
  
}