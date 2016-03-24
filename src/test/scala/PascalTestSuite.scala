
import PascalTriangle._


import org.junit._
import Assert._


class PascalTestSuite {
  @Test
  def testPascalTriangle() {
    assertEquals(1, pascalTriangle(0, 0))
    assertEquals(6, pascalTriangle(4,2))
    assertEquals(35, pascalTriangle(7,3))
  }
}