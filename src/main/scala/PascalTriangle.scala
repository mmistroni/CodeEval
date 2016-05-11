

object PascalTriangle {
  
  def pascalTriangle(row:Int, col:Int):Int = {
     if (row == col) 1 
     else if (row < 0 || col < 0 || col > row) 0 
     else pascalTriangle(row - 1, col - 1) + pascalTriangle(row - 1, col) 
  }
  
  
  
  
}