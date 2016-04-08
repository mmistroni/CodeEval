

import scala.math._ 
    object problem26 extends App { 
      /*

			In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities. Example:

			scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)) res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...

    */ 
      def combinations(lst:List[Symbol]) = { 
        for { first <- 0 until 6 
              second <- first +1 until 6 
              third <- second + 1 until 6 } 
        yield { List(lst(first), lst(second), lst(third)) }

    }

    def permutationsRecursive3(lst: List[String]): List[String] = lst match {

    case (hd :: Nil) => List(hd) case _ => { lst.flatMap(item => permutationsRecursive3(lst.filterNot(elem=> elem == item)).take(2).map(x => item + x)) } }

    def combinations2(lst: List[Symbol]) = {

     lst.flatMap(item=> for ( y <- lst.indexOf(item) + 1 until lst.length; 
                           z <- lst.indexOf(y) + 1 until lst.length if z > y) yield List(item, lst(y), lst(z)))

    }

    val lst = List('a, 'b, 'c, 'd, 'e, 'f)

    //val sol1 = combinations(lst) println(s"Sol 1:${sol1.size}@$sol1")

    val sol2 = combinations2(lst)

    println(s"Sol 1:${sol2.size}@$sol2")

    // n = 6 // c = 2

    /**

    Suggested solution

    object P26 { // flatMapSublists is like list.flatMap, but instead of passing each element // to the function, it passes successive sublists of L. def flatMapSublistsA,B(f: (List[A]) => List[B]): List[B] = ls match { case Nil => Nil case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f) }

    def combinationsA: List[List[A]] = if (n == 0) List(Nil) else flatMapSublists(ls) { sl => combinations(n - 1, sl.tail) map {sl.head :: _} } }

    */

    
    
}
