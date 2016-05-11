/**
 * High Card: Highest value card.
One Pair: Two cards of the same value.
Two Pairs: Two different pairs.
Three of a Kind: Three cards of the same value.
Straight: All cards are consecutive values.
Flush: All cards of the same suit.
Full House: Three of a kind and a pair.
Four of a Kind: Four cards of the same value.
Straight Flush: All cards are consecutive values of same suit.
Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
The cards are valued in the order:
2, 3, 4, 5, 6, 7, 8, 9, Ten, Jack, Queen, King, Ace.
 */

case class Card(value:Int, suit:String) extends Ordered[Card] {
  override def toString = s"$value of $suit"
  
  def compare(that:Card) = this.value.compare(that.value)
}

abstract class PokerHand {
  
  def buildMap(cards:Array[Card]):Map[Int, Int] =  {
    cards.foldLeft(Map[Int, Int]())((accumulator, card) => {
            val counts = accumulator.getOrElse(card.value, 0) + 1
            accumulator + (card.value -> counts)      
    })
  }
  
  def canHandle(cards:Array[Card]):Boolean
  
  
  def gauss(inputVal:Int) = (inputVal * (inputVal+1)) / 2 
  
  def value:Int
  
  def resolveTie(leftHand:Array[Card],
                      rightHand:Array[Card]):String = {
    
    val orderedLeft = buildMap(leftHand).toList.sortWith(_._2 > _._2).map(_._1).sortWith(_ < _)
    val orderedRight = buildMap(rightHand).toList.sortWith(_._2 > _._2).map(_._1).sortWith(_ < _)
    //println(s"Ordered left:$orderedLeft}")
    //println(s"Ordered right:$orderedRight}")
    resolve(orderedLeft, orderedRight)
    
    
  }
  
  def resolve(leftHand:List[Int],
                      rightHand:List[Int]):String = {
    if (leftHand.isEmpty && rightHand.isEmpty) "none"
    else {
      if (leftHand.head > rightHand.head) "left"
      else if(leftHand.head < rightHand.head) "right"
      else resolve(leftHand.tail, rightHand.tail)
    }
  }
  
}

object HighCard extends PokerHand {
  
  def canHandle(cards:Array[Card]):Boolean = true
  
  override def value =  1
    
  override def resolveTie(leftHand:Array[Card],
                                rightHand:Array[Card]) = {
    val sortedLeft = leftHand.map(_.value).sortWith(_>_)
    val sortedRight = rightHand.map(_.value).sortWith(_ > _)
    super.resolve(sortedLeft.toList, sortedLeft.toList)
  }     
  
}

class OnePair extends PokerHand {
  
  def canHandle(cards:Array[Card]):Boolean = {
    val cardMap = buildMap(cards)      
   
    cardMap.values.count(_ == 2) == 1
  }
  
  override def value = 2
  
}

class TwoPair extends PokerHand {
  
  def canHandle(cards:Array[Card]):Boolean = {
    val cardMap = buildMap(cards)      
    cardMap.values.count(_ == 2) == 2
  }
  override def value = 3
}

class ThreeOfAKind extends PokerHand {
  
  def canHandle(cards:Array[Card]):Boolean = {
    val cardMap = buildMap(cards)      
    //println(cardMap)
    cardMap.values.count(_ == 3) == 1
  }
  override def value = 4
}

class Straight extends PokerHand {
  
  override def canHandle(cards:Array[Card]):Boolean = {
      // it's a flush
      // get total value of the cards
      val cardValues = cards.map(_.value)
      //println(cardValues.mkString(","))
      val minValue = cardValues.min
      val maxValue = cardValues.max
      gauss(maxValue) - gauss(minValue-1)   == cardValues.sum
  }
  
  override def resolveTie(leftHand:Array[Card],
                                rightHand:Array[Card]) = {
    val leftVal = leftHand.map(_.value).sum
    val rightVal = rightHand.map(_.value).sum
    if (leftVal > rightVal) "left"
    else if (rightVal > leftVal) "right"
    else "none"
    
  }
  
  override def value = 5
}



class Flush extends PokerHand {
  
  def canHandle(cards:Array[Card]):Boolean = {
    val firstCardSuit = cards(0).suit
    
    val res = cards.tail.forall {_.suit == firstCardSuit  }
    //println(res)
    res
  }
  override def value = 6
  
  override def resolveTie(left:Array[Card], right:Array[Card]) = {
    val leftVal = left.map(_.value).sum
    val rightVal = right.map(_.value).sum
    if (leftVal > rightVal) "left"
    else if (rightVal > leftVal)  "right"
    else "none"
  }
}

class FullHouse extends PokerHand {
  
  def canHandle(cards:Array[Card]):Boolean = {
    val cardMap = buildMap(cards)      
    cardMap.values.count(_ == 3) == 1 && cardMap.values.count(_ == 2) == 1
  }
  override def value = 7
}

class FourOfAKind extends PokerHand {
  
  def canHandle(cards:Array[Card]):Boolean = {
    val cardMap = buildMap(cards)      
   
    cardMap.values.count(_ == 4) == 1
  }
  
  override def value = 8
  
}

class StraightFlush extends Flush {
  
  override def canHandle(cards:Array[Card]):Boolean = {
    if (super.canHandle(cards)) {
      // it's a flush
      // get total value of the cards
      val cardValues = cards.map(_.value)
      //println(cardValues.mkString(","))
      val minValue = cardValues.min
      val maxValue = cardValues.max
      
      gauss(maxValue) - gauss(minValue-1)   == cardValues.sum
    } else {
      false
    }
    
  }
  override def value = 9
}

class RoyalFlush extends StraightFlush {
  
  override def canHandle(cards:Array[Card]):Boolean = {
    if (super.canHandle(cards)) {
      // it's a flush
      // get total value of the cards
      val cardValues = cards.map(_.value)
      val minValue = 10
      val maxValue = 14
      
      gauss(maxValue) - gauss(minValue-1)   == cardValues.sum
    } else {
      false
    }
    
  }
  override def value = 10
}


class Dealer {
  val pokerHands = List(new OnePair(), new TwoPair(), new ThreeOfAKind(), new Straight(), new Flush(), new FullHouse(),
                        new FourOfAKind(), new StraightFlush(), new RoyalFlush()).reverse
  
  def findHandForCards(cards:Array[Card]):PokerHand = {
    val hand = pokerHands.filter { hands => hands.canHandle(cards) }
    if (hand.isEmpty) HighCard
    else hand(0)
  }
  
  
  def findWinner(left:Array[Card], right:Array[Card]):String =  {
    val leftHand = findHandForCards(left)
    val rightHand = findHandForCards(right)
    //println(s"leftHand${leftHand.value}|RightHand:${rightHand.value}")
    if (leftHand.value > rightHand.value) "left"
    else if (rightHand.value > leftHand.value) "right"
    else {
      // same strategy
      //println("Resolving ties..")
      leftHand.resolveTie(left, right)
    }
    
  }
  
  
}



object PokerHands {
  
  def findValue(input:Char) = input match {
          case 'T' => 10
          case 'J' => 11
          case 'Q' => 12
          case 'K' => 13
          case 'A' => 14
          case other => other.toString.toInt
      }
  
  
  
  def generateListOfCards(stringArray:Array[String]):Array[Card] = {
    stringArray.map(stringPair =>  {
                            Card(findValue(stringPair(0)), stringPair(1).toString)
    })
  }  
}