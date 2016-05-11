

object AlphabetBlocks {

  def permutations(items: List[Char]): List[List[Char]] =  {
    if (items.size == 1) List(items)
    else {
      var res = List[List[Char]]()
      for ((item, index) <- items.zipWithIndex) {
        val (pre, pos) = items.splitAt(index)
        for (permList <- permutations(pre ::: pos.tail)) res = res ++ List(item :: permList)
      }
      res
    }
  }

  
  
  
  
  /*
  def combinations(input:List[Char], length:Int):List[String] = {
    val res = permutations(input)  
    val combis = res.map(charList => charList.slice(0, length)).distinct
    combis.map(l => l.foldLeft("")((acc, item) => acc + item))
  }
  
  def canCreateWord(word: String, numOfChars: Int, availableChars: List[Char]): Boolean = {
    val allCombinations = combinations(availableChars,numOfChars)
    allCombinations.contains(word)
  }
  * /
  */
}