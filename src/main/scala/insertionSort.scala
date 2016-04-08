

import scala.math._ 
object InsertionSort extends App {

def insertionSort(lst:Array[Int]):Array[Int] = { 
  for (i <-1 until lst.length) { 
    val x = lst(i) 
    var j = i 
    while (j > 0 && lst(j-1) > x) { 
      lst(j)= lst(j-1) 
      j = j - 1 
    } 
    lst(j) = x 
    } 
  lst

}

def insertionSort(lst:List[Int]):List[Int] = { 
  if (lst.size == 1) lst 
  else { 
    val (left, right) = (List(lst.last), lst.init) 
    insert(insertionSort(left), insertionSort(right)) }

  }

  def insert(left:List[Int], right:List[Int]): List[Int] = (left, right) match {
    case (left, List()) => left
    case (List(), right) => right
    case (h1::tl1, h2::tl2) => if (h1 > h2)  h2 :: insert(h1::tl1, tl2) 
                             else h1 :: insert(tl1, h2::tl2 )
  }

  val arr = Array(1,3,4,7,5,6)

  print(insertionSort(arr).mkString)

  val lst = List(2,1,3,4,8,5,6,7) 
  print (insertionSort(lst))

}
