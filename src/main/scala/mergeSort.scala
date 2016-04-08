

object mergesort extends App{

/**

If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed. Example:

scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

    */

    def mergeSort(lst:List[Int]):List[Int] = { 
      if (lst.size == 1) lst else { 
        val (left, right) = lst.splitAt(lst.size / 2) 
        merge(mergeSort(left), mergeSort(right)) }

    }

    def merge(left:List[Int], right:List[Int]): List[Int] = (left, right) match {

    case (left, List()) => left case (List(), right) => right case (h1::tl1, h2::tl2) => if (h1 > h2) h2 :: merge(h1::tl1, tl2) else h1 :: merge(tl1, h2::tl2 )

    }

    val inputList = List(1,3,2,6,4,5,9,7)

    print (mergeSort(inputList))

}
