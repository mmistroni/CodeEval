
import PascalTriangle._
import MinimumPathSum._
import mergesort._


import org.junit._
import Assert._
import AlphabetBlocks._

class PascalTestSuite {
  @Test @Ignore
  def testPascalTriangle() {
    
    //assertEquals(1, pascalTriangle(0, 0))
    //assertEquals(6, pascalTriangle(4,2))
    //assertEquals(35, pascalTriangle(7,3))
  }
  
  @Test @Ignore
  def testClosestPair {
    val pairList = List((2, 3), (12, 30), (40, 50), (5, 1), (12, 10), (3, 4))
    val closest = closestPair(pairList)
    println("Closest Pair is:" +  closest)
    println("MInDistance=" + findDistance(closest.head, closest.last))
    
    
    println("-------------------------------------------------------")
    
    val mapped = pairList.flatMap(p => pairList.filter(it => it != p).map(q => (p, q, findDistance(p, q)))).sortBy(_._3)
    
    
    //println(mapped)
    
    
  }
  
  @Test @Ignore
  def testQuickSort() {
    val testCases = List(
        (List(5, 2, 6, 1, 3, 4), 4),
        (List(1, 2, 3, 4), 3),
        (List(4,3,2,1), 3),
        (List(3,1,2,4), 2),
        (List(1, 3, 2, 4), 2)
        )        
        
    for ((lst, expected) <- testCases) {
      val acc = scala.collection.mutable.ListBuffer[Int]()
      quickSort(lst, acc)
      assertEquals(expected, acc.size)
    }
    
  }
  
  @Test @Ignore
  def testAlphabetBlocks() {
    val tpls = List((4,"DOG","UPZRHRINOYLCKXDHNQBAGMZI"))
  }
  
  
  @Test @Ignore
  def testMinimumPathSum() {
    val x = Array(
    Array(1, 2, 3),
    Array(4,5,6), 
    Array(7,8,9))
    //println(x.foldLeft(0)((acc, item) => acc + item.sum))
    
    println(minimumPathSum(x))
    
    val y = Array(
        Array(23,388,99,177,232,30,409),
        Array(13,435,56,361,288,450,462),
        Array(240,179,476,439,255,22,395),
        Array(279,224,206,417,317,378,435),
        Array(499,421,121,402,162,11,210),
        Array(148,224,436,271,65,304,387),
        Array(32,424,71,413,263,138,395))
    val lb = scala.collection.mutable.ListBuffer[Int]()
    
    
    for(arr <- y;
        elem <- arr) lb.append(elem)
        
    
    val dups = lb.foldLeft(Map[Int, Int]()) ((acc, item) => {
      
      val counter = acc.getOrElse(item, 0)
      acc +  (item -> (counter + 1))
    })
    
    //println("Dupes=" + dups.toList.filter(tpl => tpl._2 > 1))
        
    //println("lb size:" + lb.size)
    //println("distincts:" + lb.distinct.size)
        
    println(minimumPathSum(y))
    

  }
  
  @Test@Ignore
  def testCombis() = {
    val w = "babgbag".toList
    val perms = permutations(w).map(lst => lst.foldLeft("")((acc, item) =>  acc+item))
    println(perms)
    println(perms.filter(w => w.contains("bag")).distinct)
  }
  
  
  
  
  
  @Test @Ignore
  def testSkyLine() = {
    val sample1 = List((1,2,3),(2,4,6)
        , (4,5,5)
        ,(7,3,11)
        ,(9,2,14)
        ,(13,7,15)
        , (14,3,17)    
    
    )
        
    val sl = skyLine(sample1, List[(Int, Int, Int)]()) 
    
    val expected = List(1, 2, 2, 4, 4, 5, 5, 4, 6, 0, 7, 3, 11, 2, 13, 7, 15, 3, 17, 0)
                        
    assertEquals(expected, buildSkyLine(sample1))
    
    val sample2 = List((2,22,3),(6,12,10),(15,6,21))
    assertEquals(List(2, 22, 3, 0, 6, 12, 10, 0, 15, 6, 21, 0), buildSkyLine(sample2))
    
    val sample3 = List((1,2,6),(9,23,22),(22,6,24),(8,14,19),(23,12,30))    
    assertEquals(List(1, 2, 6, 0, 8, 14, 9, 23, 22, 6, 23, 12, 30, 0), buildSkyLine(sample3))
    
    
  }
  
  @Test
  def testLatestClosestPair() {
    val points = List(
        //(2, 3), (12, 30), (40, 50), (5, 1), (12, 10), (3, 4)
        (0, 2), (6, 67), (43, 71), (39, 107), (189, 140)
    
      )
  
    val sortedX = points.sortBy(tpl => tpl._1)
    println(sortedX)
  
    val res = closestPair2(sortedX)
    println(f"Closest distance is:$res%.4f") 
    
  }
  
  
}