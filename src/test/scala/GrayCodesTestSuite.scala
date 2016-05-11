import org.junit._
import Assert._

import PokerHands._
import Stack._
import ReverseAndAdd._
import GrayCodes._
class GrayCodesTestSuite {

  @Test  @Ignore
  def testToDecimal() {
     assertEquals(10 , "1111")
     assertEquals(11 , "1110")
     assertEquals(3 , "10")
     assertEquals(65 , "1100001")
     assertEquals(6 , "101")
  }
  
  @Test @Ignore
  def testToBinary() {
     assertEquals(15 , fromBinary("1111"))
     assertEquals(14 , fromBinary("1110"))
     assertEquals(2 , fromBinary("10"))
     assertEquals(97 , fromBinary("1100001"))
     assertEquals(5 , fromBinary("101"))
  }
  
  @Test @Ignore
  def testGrayToBin() {
    assertEquals("11101", grayToBin("10011"))
    assertEquals("10010", grayToBin("11011"))
    assertEquals("11110", grayToBin("10001"))
    assertEquals("10000", grayToBin("11000"))
  }
  
  @Test
  def testGrayToDecimal() {
    
    val testCases = List(
        "1111 | 1110",
        "10 | 1100001 | 101"
        )
    val expected = List(
        "10 | 11",
        "3 | 65 | 6"
        )
    
    for (idx <- 0 until testCases.length) {
      assertEquals(expected(idx), printGray(testCases(idx)))
    }
  }
  
  
}