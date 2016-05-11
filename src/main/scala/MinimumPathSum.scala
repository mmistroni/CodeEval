

object MinimumPathSum {
  
  def buildGraphFromArray(matrix: Array[Array[Int]]): scala.collection.mutable.Map[(Int,Int), 
                          scala.collection.mutable.ListBuffer[(Int, Int)]] = {
    val graph = scala.collection.mutable.Map[(Int, Int), scala.collection.mutable.ListBuffer[(Int, Int)]]()
    for (
      row <- 0 until matrix.size;
      col <- 0 until matrix.size
    ) {
      val lst = graph.getOrElse((row, col), scala.collection.mutable.ListBuffer[(Int, Int)]())
      val leftIdx = col + 1
      val lowIdx = row + 1
      if (leftIdx < matrix.size) {
        lst.append((row,leftIdx))
      }
      if (lowIdx < matrix.size) {
        lst.append((lowIdx, col))
      }
      graph += ((row, col) -> lst)

    }
    graph
  }

  def traverse(current: (Int, Int), 
      end: (Int,Int), 
      matrix: Array[Array[Int]], 
      currentPath: List[Int], accumulator: scala.collection.mutable.ListBuffer[Int]): Unit = {
    // TODO : amend not to build a grahp first but just loop using 
    //println(s"$current|$end")
    if (current == end) {
      val elem = matrix(current._1)(current._2)
      accumulator += (elem::currentPath).sum
    }
    val currentItem = matrix(current._1)(current._2)
    val leftIdx = current._2 + 1
    val lowIdx = current._1 + 1
    if (leftIdx < matrix.size) {
        val next = (current._1, leftIdx)     
        traverse(next, end, matrix, currentItem :: currentPath, accumulator)
    }
    if (lowIdx < matrix.size) {
      val next = (lowIdx, current._2)     
      traverse(next, end, matrix, currentItem :: currentPath, accumulator)  
    }
  }

  def minimumPathSum(multiValueArray: Array[Array[Int]]): Int = {
    // TODO: print out output and try
    val accumulator = scala.collection.mutable.ListBuffer[Int]()
    val start = multiValueArray(0)(0)
    val end = multiValueArray(multiValueArray.size-1)(multiValueArray.size-1)
    
    val graph = buildGraphFromArray(multiValueArray)
    
    traverse((0,0), (multiValueArray.size-1,multiValueArray.size-1), 
        multiValueArray,   List[Int](), accumulator)
  
    val minDistance = accumulator.sortWith(_<_).head
    minDistance
  
  }
 
  
  
}