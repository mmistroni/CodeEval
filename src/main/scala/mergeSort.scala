
import scala.math._
object mergesort extends App {

  /**
   *
   * If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed. Example:
   *
   * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
   *
   */

  def mergeSort(lst: List[Int]): List[Int] = {
    if (lst.size == 1) lst else {
      val (left, right) = lst.splitAt(lst.size / 2)
      merge(mergeSort(left), mergeSort(right))
    }

  }

  def merge(left: List[Int], right: List[Int]): List[Int] = (left, right) match {
    case (left, List()) => left 
    case (List(), right) => right 
    case (h1 :: tl1, h2 :: tl2) => if (h1 > h2) h2 :: merge(h1 :: tl1, tl2) else h1 :: merge(tl1, h2 :: tl2)
  }

  
  def buildSkyLine(inputList:List[(Int, Int, Int)]):List[Int] = {
    val orderedSkyLine = inputList.sortBy(_._1)
    println(orderedSkyLine)
    val skyLineTuples = skyLine(orderedSkyLine, List[(Int, Int, Int)]())
    val flattened = skyLineTuples.foldLeft(List[Int]())((accumulator, tpl) => accumulator ::: List(tpl._1, tpl._2, tpl._3))
    flattened.filter { item => item >=0 } ::: List(0)
    
  }
  
  
  def skyLine(inputList:List[(Int, Int, Int)], accumulator:List[(Int, Int, Int)]):List[(Int, Int,Int)] = {
    if (inputList.isEmpty) accumulator
    else {
      val firstElem = inputList.head
      if (accumulator.isEmpty) {
        skyLine(inputList.tail, firstElem::accumulator)
      } else {
        val lastItemInSkyLine = accumulator.last
        //println(s"Trying to merge $firstElem with $lastItemInSkyLine")
        skyLine(inputList.tail, accumulator.filter(item=> item != lastItemInSkyLine) ::: mergeItemToSkyLine(lastItemInSkyLine, firstElem))
      }
    }
    
  }
  
  def mergeItemToSkyLine(leftBuilding:(Int, Int, Int), nextBuilding:(Int, Int, Int)):List[(Int, Int ,Int)]= {
    val (left1, height1, right1) = leftBuilding
    val (left2, height2, right2) = nextBuilding
    // 3 cases. -1 will be used to replace irrelevant measure
    if (right1 == left2) {
      List((left1, height1, right1), (-1, height2, right2))
    }
    else if (left2 < right1 && right1 < right2) {
      // buildings intersects
      if (height1 < height2) List((left1, height1, left2), (-1, height2, right2))
      else List((left1, height1, right1), (-1, height2, right2))
    }
    else if (left1 < left2 && right2 < right1) {
      // one building embedded into another
      List((left1, height1, left2),( -1, height2, right2), (-1, height1, right1))
    }
    else {
      // building disjointed from each other
      List((left1, height1, right1), (-1,-1,0), (left2, height2, right2))
    } 
  }
  
  
  def skyScrapers(input:List[(Int, Int, Int)]):List[(Int, Int,Int)] = {
    // TODO replace recursively.should be easier
    if (input.size == 1) input
    else {
      val (left, right) = input.splitAt(input.size / 2)
      mergeSkyLine(skyScrapers(left), skyScrapers(right))
    }
  }
  
  def mergeSkyLine(leftSkyLine:List[(Int, Int, Int)], 
                   rightSkyLine:List[(Int, Int, Int)]):List[(Int, Int, Int)] =  {
    println(s"${leftSkyLine}|${rightSkyLine}")
    if (rightSkyLine.isEmpty) leftSkyLine
    else if(leftSkyLine.isEmpty) rightSkyLine
    else {
      val (left1, height1, right1)  = leftSkyLine.head
      val (left2, height2, right2) = rightSkyLine.head
   
      if (left2 < right1 && right1 < right2) {
        // simple intersection
        println("Simple intersection")
        (left1, height1, left2) :: mergeSkyLine((-1, height2, right2) :: leftSkyLine.tail, rightSkyLine.tail) 
      } else{
        //if (left2 < right1 && right2 < right1){
        println(s"ONe embedded into another.Head1${leftSkyLine.head}|${rightSkyLine.head}")
        // one building embedded into another
        (left1, height1, left2) :: List((left2, height2, right2) 
                                    ,(-1, height1, right1)
                                    ) ::: mergeSkyLine(leftSkyLine.tail,
                                                  rightSkyLine.tail)
      }/** 
       else {
        // building are disjointed. 
        (left1, height1, right2) :: List((-1, -1,0)) ::: mergeSkyLine(leftSkyLine.tail, rightSkyLine.tail)
      }**/
       
    }
    
  }
  
  def quickSort(lst: List[Int], acc:scala.collection.mutable.ListBuffer[Int]): List[Int] = {
    import scala.util.Random
    if (lst.size <=1) lst
    else {
      val r = new Random
      val rIdx = r.nextInt(lst.size -1)
      val pivot = lst.last //(rIdx) // lst.last
      acc += pivot
      val loList = lst.filter(_ < pivot)
      val hiList = lst.filter(_ > pivot)
      quickSort(loList, acc) ::: List(pivot) :::  quickSort(hiList, acc)
    }
  }
  
  def combine(left: List[Int], pivot:Int,  right: List[Int]): List[Int] =  {
    left ::: List(pivot) ::: right
  }
  
  
  
  import scala.math._
  
  def findDistance(p:(Int ,Int), q:(Int, Int)):Double = {
    val (px, py) = p
    val (qx, qy) = q
    math.sqrt(math.pow(px-qx, 2) + math.pow(py-qy,2))
  }
  
  
  
  
  def bruteForce(inputList:List[(Int, Int)]):Double = {
      var  min = Double.MaxValue
    for (i <- 0 until inputList.size;
         j <- i+1 until inputList.size)
         if (findDistance(inputList(i),inputList(j)) < min) {
           min = findDistance(inputList(i),inputList(j)) 
         }  
    min
  } 
  
  
  
  def stripClosest( strip:List[(Int, Int)], size:Int, d:Double):Double = {
    var min = d;  // Initialize the minimum distance as d
 
    val sorted = strip.sortBy(tpl => tpl._2)
    
    // Pick all points one by one and try the next points till the difference
    // between y coordinates is smaller than d.
    // This is a proven fact that this loop runs at most 6 times
    for (i <- 0 until size;
         j <- i+1 until size if sorted(j)._2- sorted(i)._2 < min)
            if (findDistance(sorted(i),sorted(j)) < min)
                min = findDistance(sorted(i),sorted(j))
 
    return min;
}
  
  
  def closestPair2(xList:List[(Int, Int)]):Double = {
    // TO BE CONTINUED  xList should be sorted on X
    if (xList.size <=3) bruteForce(xList)
    else {
      val xLeft = xList.slice(0, xList.size / 2)
      val xRight = xList.slice(xList.size / 2 + 1 , xList.size)
      val xMiddle = xLeft(xLeft.size / 2)
      val dL = closestPair2(xLeft)
      val dR = closestPair2(xRight)
      
      val minDistance = min(dL, dR)
      
      val closestStrip = for (i <- 0 until xList.size
                              if abs(xList(i)._1 - xMiddle._1) < minDistance) 
                                    yield xList(i) 
      
      
      min(minDistance, stripClosest(closestStrip.toList, closestStrip.size,
                                            minDistance))
    }
  }
  
  def combineX(left: List[(Int, Int)], right:List[(Int, Int)] ): List[(Int, Int)] = {
    val leftMinDistance = left.flatMap(p => left.filter(it => it != p).map(q => (p, q, findDistance(p, q)))).sortBy(tpl => tpl._3)
    val rightMinDistance = right.flatMap(p => right.filter(it => it!= p).map(q => (p, q, findDistance(p, q)))).sortBy(tpl => tpl._3)
    val leftRightDistance = left.flatMap(p => right.map(q => (p, q, findDistance(p, q)))).sortBy(tpl => tpl._3)
    
    val combined = (leftMinDistance ::: rightMinDistance ::: leftRightDistance).sortBy(_._3)
    
    List(combined.head._1, combined.head._2)
    
    
  }
  
  def closestPair(inputList:List[(Int, Int)]):List[(Int, Int)] = {
    if (inputList.size < 2) inputList
    else {
      val sortedList = inputList.sortBy(_._1)
      val (left, right) = sortedList.splitAt(sortedList.size / 2)
      println(s"LeftSplit is:$left\n--------")
      println(s"RightSplit is:$right")

      combineX(closestPair(left), closestPair(right))
    }
  }
  
  
  
  val inputList = List(1, 3, 2, 6, 4, 5, 9, 7)

  print(mergeSort(inputList))

}
