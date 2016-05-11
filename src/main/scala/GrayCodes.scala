

object GrayCodes {
  
  def toDecimal(grayNum:String):Int = {
    0
  }
  
  def fromBinary(binaryString:String): Int = {
    import scala.math._
    val reversed  = binaryString.toList.map(_.toString.toInt).reverse.zipWithIndex
    reversed.foldLeft(0)((acc, tpl) => {
      val (digit, idx) = tpl
      acc + (digit * pow(2, idx).toInt)
    })
  }
  
  def grayToBin(binary:String):String = {
    val binaryString = binary.toList.map(_.toString)
    var binaryList = List(binaryString(0))
    for (i <- 1 until binaryString.length) {
      val res = binaryList(i-1).toInt ^ binaryString(i).toInt
      binaryList = binaryList :+ res.toString()
    }
    binaryList.mkString
        
  }
  
  
  
  
  def printGray(input:String):String = {
    val numStrings = input.replace(" ", "").split('|')
    println(numStrings.mkString(","))
    val result = for(binaryString <- numStrings) yield {
              val binary = grayToBin(binaryString)
              fromBinary(binary)
          }
    result.mkString(" | ")
  }
  
  def grayToBinRecursive(binary:String):String = {
    println(binary)
    if (binary.length() == 1) binary.charAt(0).toString()
    else {
      (binary.charAt(binary.length - 2).toInt ^ binary.last.toInt).toString + grayToBinRecursive(binary.init)
    }
  }
  
  
}