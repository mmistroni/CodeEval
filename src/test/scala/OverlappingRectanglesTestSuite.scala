import org.junit._

import Assert._

import OverlappingRectangles._

class OverlappingRectangleTestSuite {

  @Test 
  def testOverlappingRectangles() {
    
    val usecases = List(
        "-3,3,-1,1,1,-1,3,-3"
        ,"-3,3,-1,1,-2,4,2,2"
        )
    val expected  = List(
        false, 
        true) 
        
    for (idx <- 0 until usecases.size) {
      val (arr1, arr2) = usecases(idx).split(",").map(_.toInt).splitAt(4)
      val res = overlap((arr1(0), arr1(1), arr1(2),arr1(3)),
                        (arr2(0), arr2(1), arr2(2),arr2(3)))
      assertEquals(expected(idx), res)
      println(s"UseCase:$idx passed")    
    }
  }
  
   
  
}