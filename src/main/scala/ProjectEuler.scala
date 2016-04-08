

import scala.math._

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.control.Exception.By
import scala.collection.mutable.PriorityQueue
import scala.collection._

object ProjectEuler extends App {

  def permutations(items: List[Int]): List[Int] = items match {
    case Nil => List[Int]()
    case h :: t => {
      var res = List[Int]()
      for ((item, index) <- items.zipWithIndex) {
        val (pre, pos) = items.splitAt(index)
        for (permList <- permutations(pre ::: pos.tail)) res = res ++ List(item + permList)
      }
      res
    }
  }

  def binary(num: Int): String = {
    def base2(num: Int): List[Int] = {
      if (num / 2 == 0) List(num)
      else {
        val rem = num % 2
        val newnum = num / 2
        rem :: base2(newnum)
      }
    }
    base2(num).mkString
  }

  def isPalyndrome(num: Int) = {
    val decStr = num.toString
    if (num.toString == num.toString.reverse) {
      val binNum = binary(num)
      binNum == binNum.reverse
    } else {
      false
    }
  }

  def problem36(): Int = {
    val res = for (n <- (1 until 1000000) if isPalyndrome(n)) yield n
    return res.reduce { (acc, item) => acc + item }
  }

  def isPandigit(num: Int): Boolean = {
    val digitSeq = num.toString.map(_.asDigit).distinct
    digitSeq.size == 9
  }

  def products(maxRange: Int): Seq[(Int, Int, Int)] = {

    val res = for (
      multiplicand <- 1 to 5;
      multiplier <- 1 to 5
    ) yield { (multiplicand, multiplier, multiplicand * multiplier) }
    res

  }

  def problem32(): Int = {
    //We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; 
    //for example, the 5-digit number, 15234, is 1 through 5 pandigital. 
    //The product 7254 is unusual, as the identity, 39 × 186 = 7254, 
    //containing multiplicand, multiplier, and product is 1 through 9 pandigital. 
    //Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital 
    // multiplicand multiplier plus product shud be concatenated, taken as a set and verify length is 9 
    // it means that the lenght of multiplicand+multiplier+product must be 9, otherwise we skip 1

    //val res = for( multiplicand <- 1 to 1000,
    //     multiplier  <- 1 to 1000,
    //     if  multiplicand * multiplier < 987654321) yield {multiplicand, multiplier}
    1
  }

  def parenthesisBalanced(strList: List[Char]): Boolean = {

    def _innerBalanced(count: Int, resList: List[Char]): Boolean = (count, resList) match {

      case (0, Nil) =>
        println("zero case"); true
      case (-1, _) =>
        println("minus one"); false
      case (x: Int, h :: tl) => {
        println("standard case for:" + x);
        if (h == ')') _innerBalanced(x - 1, tl)
        else if (h == '(') {
          println("adding 1 to open parenthesis")
          _innerBalanced(x + 1, tl)
        } else
          _innerBalanced(x, tl)
      }
    }

    _innerBalanced(0, strList)
  }

  def isPrime(n: Int) = { (2 to sqrt(n).toInt).forall(n % _ != 0) }

  def primesUpTo(n: Int) = {
    for (candidate <- 2 to n if isPrime(candidate)) yield candidate

  }

  def pFactors(n: Int) = {

    def _primeFactorsFor(candidate: Int, primeList: List[Int]): List[Int] = (candidate, primeList) match {

      case (1, h :: tl) => List[Int]()
      case (num, h :: tl) => if (num % h == 0) {
        h :: _primeFactorsFor(num / h, primeList)
      } else {
        _primeFactorsFor(num, tl)
      }

    }
    val primes = primesUpTo(n)
    _primeFactorsFor(n, primes.toList).distinct

  }

  def isConsecutive(first: Int, second: Int): Boolean = { second - first == 1 }

  def consecutivePrimeFactors(numOfDistinctPrimes: Int, start: Int, resultSize: Int): List[Int] = {

    def _primeFactors(n: Int, currentResult: List[Int]): List[Int] = {
      val factors = pFactors(n)
      if (currentResult.size == resultSize) {
        currentResult
      } else {
        if (factors.size == numOfDistinctPrimes) {
          if (currentResult.isEmpty) {
            _primeFactors(n + 1, currentResult ::: List(n))
          } else {
            if (isConsecutive(currentResult.last, n)) {
              _primeFactors(n + 1, currentResult ::: List(n))
            } else {
              println("Skipping. " + currentResult.last + " and " + n + " are not consecutive")
              _primeFactors(n + 1, List[Int]())
            }
          }
        } else {
          _primeFactors(n + 1, List[Int]())
        }

      }
    }

    _primeFactors(start, List[Int]())

  }

  def createCustomFutureFor[A](f: => A): Future[A] = {
    Future { f }
  }

  def testFun(numOfPrimes: Int, resultSize: Int, start: Int): List[Int] = {

    def createFutureFor(f: => List[Int]): Future[List[Int]] = {
      Future {
        f
      }
    }

    val solutionFunction = consecutivePrimeFactors(numOfPrimes, _: Int, resultSize)
    val firstFut = createCustomFutureFor { solutionFunction(10) }
    val secondFut = createCustomFutureFor { solutionFunction(10000) }

    val firstComplete = Future.firstCompletedOf(List(firstFut, secondFut))

    val res = Await.result(firstComplete, 1.day)
    //#satisfiesAll(startingTriangle)
    println("Out")
    res

    //consecutivePrimeFactors(numOfPrimes, start, resultSize)

  }

  def problem47(numOfPrimes: Int, resultSize: Int, start: Int): List[Int] = {
    List(1, 2, 3)
  }

  def createFutureFor(startPoint: Double, f: Double => (Double, Double)): Future[(Double, Double)] = Future {
    satisfiesAll(startPoint)
    f(startPoint)
  }

  def problem45(startingTriangle: Double): (Double, Double) = {

    val firstFut = createFutureFor(startingTriangle, satisfiesAll)
    val secondFut = createFutureFor(500000000, satisfiesAll)

    val firstComplete = Future.firstCompletedOf(List(firstFut, secondFut))

    val res = Await.result(firstComplete, 1.day)
    //#satisfiesAll(startingTriangle)
    res

  }

  def satisfiesAll(n: Double): (Double, Double) = {
    //println(s"Checking if Triangle($n) satisfies all conditions") 
    //
    val trian = Triangle(n)
    val hex = Hex(n)
    if (Pentagonal(hex))
      (hex, n) //&& Hexagonal(trian)) else satisfiesAll(n + 1)
    null
  }

  def Triangle(n: Double): Double = { n * (n + 1) / 2 }

  def Hex(n: Double): Double = { n * (2 * n - 1) }

  def Pentagonal(n: Double): Boolean = {
    val pent = (sqrt((24 * n.toDouble) + 1) + 1) / 6
    pent - floor(pent) == 0
  }

  def Hexagonal(n: Double): Boolean = {
    val hex = (sqrt(8 * n.toDouble + 1) + 1) / 4
    hex - floor(hex) == 0

  }

  def problem50(limit: Int): Int = {

    /**
     * The prime 41, can be written as the sum of six consecutive primes:
     * 41 = 2 + 3 + 5 + 7 + 11 + 13
     *
     * This is the longest sum of consecutive primes that adds to a prime below one-hundred.
     *
     * The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
     *
     * Which prime, below one-million, can be written as the sum of the most consecutive primes?
     */

    val allPrimes = primesUpTo(limit).toList

    //val myfut = createCustomFutureFor {sumOfMostConsecutivePrimes(allPrimes, limit)}

    val res = sumOfMostConsecutivePrimes(allPrimes, limit) // Await.result(myfut, 1.day)

    res

  }

  def sumOfMostConsecutivePrimes(primes: List[Int], limit: Int): Int = {

    def findConsecutiveSums(primeList: List[Int], currentResult: List[Int]): List[Int] = {
      if (primeList.isEmpty || primeList.head > limit) {
        currentResult
      } else {
        val currentSum = primeList.reduceLeft((acc, item) => if (acc + item < limit) acc + item else acc)
        if (primes.contains(currentSum)) {
          findConsecutiveSums(primeList.tail, currentSum :: currentResult)
        } else {
          findConsecutiveSums(primeList.tail, currentSum :: currentResult)
        }

      }
    }

    val res = findConsecutiveSums(primes, List[Int](0)).filter(item => primes.contains(item))
    res.last

  }

  def problem46(): Int = {

    /**
     *
     * It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
     *
     * 9 = 7 + 2×12
     * 15 = 7 + 2×22
     * 21 = 3 + 2×32
     * 25 = 7 + 2×32
     * 27 = 19 + 2×22
     * 33 = 31 + 2×12
     *
     * It turns out that the conjecture was false.
     *
     * What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
     *
     */

    def goldblachOther(oddNumber: Int): Int = {
      if (isPrime(oddNumber)) {
        goldblachOther(oddNumber + 2)
      } else {
        // get all the primes up to the number
        // for each prime subtract the prime from the odd, split in two and check if remainder is a perfect root
        val primesDivisors = primesUpTo(oddNumber)
        val res = primesDivisors.exists(item => {
          val sq = sqrt((oddNumber - item) / 2)
          sq - floor(sq) == 0
        })
        if (!res) {
          println(s"$oddNumber does not satisfy goldblach")
          oddNumber
        } else {
          goldblachOther(oddNumber + 2)

        }
      }
    }

    val res = goldblachOther(3)
    println(res)
    res

  }

  def problem52(): Int = {
    /** * It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order. * * Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits. * */

    val solutionFunction = permutedMultiples(_: Int)
    val firstFut = createCustomFutureFor { solutionFunction(1) }
    val secondFut = createCustomFutureFor { solutionFunction(10000) }
    val thirdFut = createCustomFutureFor { solutionFunction(100000) }

    val firstComplete = Future.firstCompletedOf(List(firstFut, secondFut, thirdFut))

    val res = Await.result(firstComplete, 1.day)
    println("OUt");
    res

  }

  def permutedMultiples(n: Int): Int = {
    if (checkMultiples(n)) n
    else permutedMultiples(n + 1)
  }

  def checkMultiples(starter: Int): Boolean = {
    (hasSameDigits(starter, starter * 2)
      && hasSameDigits(starter, starter * 3)
      && hasSameDigits(starter, starter * 4)
      && hasSameDigits(starter, starter * 5)
      && hasSameDigits(starter, starter * 6))
  }

  def hasSameDigits(original: Int, candidate: Int): Boolean = {
    val first = original.toString.toList
    val second = candidate.toString.toList
    if (first.size == second.size) first.forall(second.contains(_))
    else
      false
  }

  class Node(val id: String, val value: Int, val left: Node = null, val right: Node = null) {

    def isLeaf(): Boolean = {
      (left == null) && (right == null)
    }

    override def toString() = s"id:$id|val=$value|left=$left|right=$right"

  }

  def huffmanCodes(): PriorityQueue[Node] = {
    /** * First of all, consult a good book on discrete mathematics or algorithms * for a detailed description of Huffman codes! * We suppose a set of symbols with their frequencies, given as a list of (S, F) Tuples. * E.g. (("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)). * Our objective is to construct a list of (S, C) Tuples, where C is the Huffman code word for the symbol S. * * scala> huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))) * res0: List[String, String] = List((a,0), (b,101), (c,100), (d,111), (e,1101), (f,1100)) * */

    /**
     * alphabet = a, b,c,d,e
     * frequencies = .45, .13, .12, .16, .09, .05
     *
     * here's the algorithm:
     *
     * Create a leaf node for each symbol and add it to the priority queue.
     * While there is more than one node in the queue:
     * Remove the two nodes of highest priority (lowest probability) from the queue
     * Create a new internal node with these two nodes as children and with probability equal to the sum of the two nodes' probabilities.
     * Add the new node to the queue.
     * The remaining node is the root node and the tree is complete.
     *
     * So, shortly, the paper algorithm is
     * while there is more than 1 node in the queue:
     * - find the two lowest priority and create a new node which is the sum of the two
     * - remove the two lowest and add the new node to the queue
     * - repeat the process
     * It seems we need two data structures:
     *     - a priority queue to keep track of current priorities. we will remove the two lowesd and add the new node
     *     - another priority queue to which we can add the removed nodes and the new node
     *
     *
     *
     *
     *
     */

    val pq = PriorityQueue.empty[Node](
      Ordering.by((_: (Node)).value).reverse)

    val tupleList = List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))

    val nodeList = tupleList.map(item => new Node(item._1, item._2, null, null))
    nodeList.foreach(pq.enqueue(_))
    pq

  }

  def problem51(): (Int, List[Int]) = {
    // TODO THIS NEED TO BE COMPLETED!! 
    /**
     * * By replacing the 1st digit of the 2-digit number 3,
     *   * it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83,
     *   are all prime. * By replacing the 3rd and 4th digits of 56*3 with the same digit,
     *
     *
     *   * this 5-digit number is the first example having seven primes
     *   * among the ten generated numbers, yielding the family:
     *   * 56003, 56113, 56333, 56443, 56663, 56773, and 56993.
     *   * Consequently 56003, being the first member of this family,
     *    is the smallest prime with this property. * Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.
     */

    // First off write a rountine that proves  the example
    // reuse the function primesUpTo , and do it up to 70k

    // to revisit
    // 1019 should not become 1033.

    // Then work your way to satisfy the problem
    // Find all primes up to 1k
    // for each of the primes . for now we assume we need to replace it only twice

    val range = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

    def generate(num: String, replacement: Char): Seq[String] = {
      println("NUm is:" + num)
      // Broken
      //  we need to replace all the digit, not only two at  time
      // smarter. a digit that is already contained in the start of the number might not count
      val start = num(0)
      for (
        idx <- 1 until num.size - 1;
        idx2 <- idx + 1 until num.size - 1
      ) yield {
        val tmp = num.toArray;
        tmp(idx) = replacement;
        tmp(idx2) = replacement;
        tmp.foldLeft("")((acc, item) => acc + item)

      }
    }

    println("Finding max prime with combinations...");
    val primesUpTo50k = primesUpTo(60000).filter(_ > 100)
    println("Starting the check....")
    def findMax(candidateList: Seq[Int]): (Int, List[Int]) = {
      for (candidate <- primesUpTo50k) {
        val candidates = for (digit <- range) yield generate(candidate.toString, digit);
        val primeCandidates = for (cand <- candidates.flatten if isPrime(cand.toInt)) yield cand.toInt
        println(primeCandidates.distinct)
        println(s"$candidate: we have ${primeCandidates.size}")
        if (primeCandidates.distinct.size == 8) {
          println("Break. we got it!" + candidate)
          println("Break. we got it!" + primeCandidates)
          return (candidate, primeCandidates.toList)
        }
      }
      return (-1, List(-1))

    }

    findMax(primesUpTo50k)

  }

  def palyndrome(num: BigDecimal) = BigDecimal(num.toString.reverse)

  def isPalynd(num: BigDecimal): Boolean = {
    val numStr = num.toString
    numStr == numStr.reverse
  }

  def isLychrel(candidate: Int): Boolean = {
    println(s"Checking if $candidate is Lychrel")

    def producePalyndr(num1: BigDecimal, count: Int): Boolean = {
      //println("Count:" + count) 
      if (count > 49) true
      else {
        //println("Candiate:" + num1) 
        val palynd = palyndrome(num1) //println("palnyd:" + palynd) 
        val sum = num1 + (palynd)
        if (isPalynd(sum)) false
        else {
          producePalyndr(sum, count + 1)
        }
      }
    }

    producePalyndr(BigDecimal(candidate.toString), 1)

  }

  def problem53(): Int = {
    val lychrel = for (num <- 1 until 10000 if isLychrel(num)) yield num
    lychrel.size
  }

  def gcd(num1: Int, num2: Int): Int = { if (num2 == 0) num1 else gcd(num2, num1 % num2) }

  def isRelativelyPrimeWith(num: Int, candidate: Int): Boolean = { gcd(num, candidate) == 1 }

  def totient(num: Int): Int = {
    val primesWith = for (cand <- 2 until num if isRelativelyPrimeWith(num, cand))
      yield cand
    (1 :: primesWith.toList).size
  }

  def phi(num: Int): Double = {
    println("finding phi of:" + num)
    val tot = totient(num).toDouble
    num / tot
  }

  def problem69(): (Int, Double) = {

    /**
     * It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.
     *
     * Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
     *
     */

    val solutionFunction = maximumPhi(1: Int, 1000001: Int)
    val firstFut = createCustomFutureFor { solutionFunction }
    //val secondFut = createCustomFutureFor { solutionFunction(10000) }
    //val thirdFut = createCustomFutureFor { solutionFunction(100000) }

    //val firstComplete = Future.firstCompletedOf(List(firstFut, secondFut, thirdFut))

    val res = Await.result(firstFut, 1.day)
    println("OUt");

    res

  }

  def maximumPhi(start: Int, end: Int): (Int, Double) = {
    val res = for (candidate <- start until end) yield (candidate, phi(candidate))

    res.sortBy(item => item._2).reverse.head

  }

  def generate4DigitPolynomial(start: Int, formula: Int => Int, acc: List[Int]): List[String] = {
    val res = generatPolynomial(start, formula)
    if (res > 9999) {
      acc.filter(_ > 999).map(_.toString)
    } else {
      generate4DigitPolynomial(start + 1, formula, acc ::: List(res))
    }
  }

  def generatPolynomial(num: Int, formula: Int => Int): Int = formula(num)

  def problem61(): List[Int] = { List(1, 2, 3) }

  def findDigitSum(numStr: String): Int = {
    val listWithNoNines = numStr.filter(_ != '9')
    val sum = listWithNoNines.foldLeft(0)((acc, item) => acc + item.toString.toInt)
    if (sum > 10) {
      val res = sum.toString.foldLeft(0)((acc, item) => acc + item.toString.toInt)
      res
    } else sum

  }

  def isPerfectSquare(candidate: BigDecimal): Boolean = {
    println(s"Checking if $candidate is perfect square")
    val candidateStr = candidate.toString
    val zeros = candidateStr.count(_ == '0')
    val knownSquareSuffixes = List('1', '4', '5', '6', '9')
    val knownSquareDigitalRoots = List(1, 4, 7, 9)
    if (knownSquareSuffixes.contains(candidateStr.last) || zeros % 2 == 0) {
      val root = sqrt(candidate.toDouble)
      if (root - root.toInt > 0.0) false else true

    } else false

  }

  def findDiophantineSolution(dTermStart: Int, dTermLimit: Int): Seq[(Int, BigDecimal)] = {
    for (d <- dTermStart to dTermLimit) yield (d, findSol(d))
  }

  def findSol(dTerm: Int, startPoint: BigDecimal = BigDecimal(1)): BigDecimal = {
    println(s"Finidng solution for d=$dTerm and startPoint=$startPoint")
    if (isPerfectSquare(dTerm.toDouble))
      findSol(dTerm + 1)
    else if (startPoint > BigDecimal("1819380158564165")) -1.0
    else {
      val sol = (startPoint * startPoint * dTerm + 1)
      println(s"Find sq root of:$sol")
      if (isPerfectSquare(sol)) {
        println(s"returning square root of $sol")
        sqrt(sol.toDouble)
      } else findSol(dTerm, startPoint + 1)
    }
  }

  def problem66(): Int = {

    1

  }

}










    
    
