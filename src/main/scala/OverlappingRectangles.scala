

object OverlappingRectangles {
  
  
  def overlap(rect1:(Int, Int, Int, Int), rect2:(Int, Int, Int, Int)): Boolean = {
    val (aX, aY, cX, cY) = rect1
    val (a1X, a1Y, c1X, c1Y) = rect2
    
    if (aX <= a1X) {
      // rect1 on the left
      // case1: a2Y > aY. then we 
      if (aY <= a1Y) {
        (a1X >= aX && a1X <= cX) && (c1Y <= aY)
      } else {
        (a1X >= aX && a1X <= cX) && (a1Y >= cY)
      }
    } else {
      
      if (a1Y >= aY) {
        (c1X >= aX   && c1X <= cX) && (c1Y >= cY)
      } else {
        (aX >= a1X  &&   aX <= c1X) && ( a1Y >= cY)
      }
    } 
  }
    
  
}