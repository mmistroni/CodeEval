

object problem62Euler extends App {

  def generateCube(candidate: scala.math.BigDecimal): List[(Char, Int)] = {
    val cube = candidate * candidate * candidate * 1l
    val cubeStr = cube.toString
    val mp = cubeStr.foldLeft(Map[Char, Int]())((acc, item) => {
      val newcounter = acc.getOrElse(item, 0) + 1
      acc + (item -> newcounter)
    })

    mp.toList.sortBy(_._1)
  }

  def findMaxCube(cand: Int, dict: Map[List[(Char, Int)], List[Int]]): List[Int] = {
    val cbList = generateCube(BigDecimal(cand))
    val itemInDict: List[Int] = dict.getOrElse(cbList, List[Int]())
    val newItemInDict: List[Int] = cand :: itemInDict
    if (newItemInDict.size == 5) return newItemInDict else findMaxCube(cand + 1, dict + (cbList -> newItemInDict))

  }

  def problem62(): List[Int] = {

    val mapper = Map[List[(Char, Int)], List[Int]]()

    val res = findMaxCube(1, mapper)

    println(res)

    res
    // The cube, 41063625 (3453), can be permuted to produce two other cubes: 56623104 (3843) and 66430125 (4053). 
    // In fact, 41063625 is the smallest cube which has exactly three permutations of 
    // its digits which are also cube.

    //Find the smallest cube for which exactly five permutations of its digits are cube.

    // 1. find a rule to find all the cubes
    // 2. create dictionary according to the number of digits (we use them  to find permutations)
    // 3. we loop thru dict to find the key with 5 values

  }
}