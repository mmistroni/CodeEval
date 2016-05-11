

object ReverseAndAdd {
  
  
  def reverseAndAdd(num:Int):(Int, Int) = {
    
    def isPalyndrome(num:Int):Boolean = {
      num.toString.equals(getReverse(num).toString)
    }
    
    def getReverse(num:Int):Int = {
      num.toString.reverse.toInt
    }
    
    def reverseAndCount(num:Int, iter:Int) :(Int, Int) = {
      val result = num + getReverse(num)
      if (isPalyndrome(result)) (iter + 1, result)
      else
        reverseAndCount(result,iter +1)
    }
    
    reverseAndCount(num, 0)
  }
}