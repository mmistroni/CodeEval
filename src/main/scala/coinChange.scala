

object CoinChange extends App {

  def coinChange(change: Int, lst: List[Int]): Int = {

    if (change == 0) 1
    else if (change < 0 || lst.isEmpty) 0
    else
      coinChange(change, lst.tail) + coinChange(change - lst.head, lst)

  }

}
