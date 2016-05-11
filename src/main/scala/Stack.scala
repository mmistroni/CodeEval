

object Stack {
  
  def createStack(inputList:List[String]):List[String] = {
    inputList.reverse.zipWithIndex.filter(tpl => tpl._2 % 2 == 0).map(_._1)
    
  }
  
  
}