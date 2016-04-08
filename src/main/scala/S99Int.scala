

import scala.math._
package arithmetic {
  object S99Int { implicit def int2S99Int(i: Int): S99Int = new S99Int(i) } 

  class S99Int(val start: Int) {
    import S99Int._

    def isPrime(): Boolean = {

      val coll = for (idx <- 3 until (start / 2) + 1 by 2) yield {
        start % idx == 0
      }
      !coll.exists(_ == true)
    }

    def isPrime2() = {
      val lst = (3 until (start / 2) + 1 by 2).toList
      lst.forall(item => start % item != 0)
    }

  }
}
