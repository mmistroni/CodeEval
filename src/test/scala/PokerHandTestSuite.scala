import org.junit._
import Assert._

import PokerHands._

class PokerHandTestSuite {

  @Test @Ignore 
  def testGenerateFrequencies() {
    val twoPlayerCards ="6D 7H AH 7S QC 6H 2D TD JD AS"
    
    val (player1, player2) = twoPlayerCards.split(" ").splitAt(5)
    
    println (s"Player1. Input:${player1.mkString(",")} => ${generateListOfCards(player1).mkString(":")}")
    
  }  
   
  @Test @Ignore
  def testPokerHandsOnePair() {
    val twoPlayerCards ="6D 7H AH 7S QC 6H 2D TD JD AS"
    
    val (player1, player2) = twoPlayerCards.split(" ").splitAt(5)
    println (s"Player1. Input:${player1.mkString(",")} => ${generateListOfCards(player1).mkString(":")}")
    
    val onePairStrategy = new OnePair()
    val twoPairStrategy = new TwoPair()
    assertTrue(onePairStrategy.canHandle(generateListOfCards(player1)))
    assertFalse(twoPairStrategy.canHandle(generateListOfCards(player1)))
    assertFalse(onePairStrategy.canHandle(generateListOfCards(player2)))
    assertFalse(twoPairStrategy.canHandle(generateListOfCards(player2)))
    
  }
  
  @Test @Ignore
  def testOnePairLeftWinner() {
    val twoPlayerCards ="6D 7H AH 7S QC 6H 6D TD JD AS"
    
    val (player1, player2) = twoPlayerCards.split(" ").splitAt(5)
    println (s"Player1. Input:${player1.mkString(",")} => ${generateListOfCards(player1).mkString(":")}")
    
    val dealer = new Dealer()
    val onePairStrategy = new OnePair()
    val twoPairStrategy = new TwoPair()
    
    val leftHand = generateListOfCards(player1)
    val rightHand = generateListOfCards(player2)
    
    assertTrue(onePairStrategy.canHandle(leftHand))
    assertTrue(onePairStrategy.canHandle(rightHand))
    assertEquals("left", dealer.findWinner(leftHand, rightHand))
  }
  
  @Test @Ignore
  def testPokerHandsTwoPair() {
    val twoPlayerCards ="6D 7H AH 7S QC JH 2D TD JD 2D"
    
    val (player1, player2) = twoPlayerCards.split(" ").splitAt(5)
    println (s"Player1. Input:${player1.mkString(",")} => ${generateListOfCards(player1).mkString(":")}")
    
    val twoPairStrategy = new TwoPair()
    assertFalse(twoPairStrategy.canHandle(generateListOfCards(player1)))
    assertTrue(twoPairStrategy.canHandle(generateListOfCards(player2)))
    
  }
  
  @Test @Ignore
  def testPokerHandsThreeOfAKind() {
    val twoPlayerCards ="6D 7H 7J 7S QC JH 2D TD JD 2D"
    
    val (player1, player2) = twoPlayerCards.split(" ").splitAt(5)
    println (s"Player1. Input:${player1.mkString(",")} => ${generateListOfCards(player1).mkString(":")}")
    
    val dealer = new Dealer()
    val onePairStrategy = new OnePair()
    val twoPairStrategy = new TwoPair()
    
    val leftHand = generateListOfCards(player1)
    val rightHand = generateListOfCards(player2)
    
    val tokStrategy = new ThreeOfAKind()
    assertTrue(tokStrategy.canHandle(generateListOfCards(player1)))
    assertFalse(tokStrategy.canHandle(generateListOfCards(player2)))
    
    assertEquals("left", dealer.findWinner(leftHand, rightHand))
    
  }
  
  @Test @Ignore
  def testPokerHandsFlush() {
    val twoPlayerCards ="6D 7H 7J 7S QC JH 2H TH QH 3H"
    
    val (player1, player2) = twoPlayerCards.split(" ").splitAt(5)
    println (s"Player1. Input:${player1.mkString(",")} => ${generateListOfCards(player1).mkString(":")}")
    
    val dealer = new Dealer()
    
    val leftHand = generateListOfCards(player1)
    val rightHand = generateListOfCards(player2)
    
    val flushStrategy = new Flush()
    assertFalse(flushStrategy.canHandle(generateListOfCards(player1)))
    assertTrue(flushStrategy.canHandle(generateListOfCards(player2)))
    
    assertEquals("right", dealer.findWinner(leftHand, rightHand))
    
  }
  
  @Test @Ignore
  def testFullHouse() {
    val twoPlayerCards ="6D 7H 7J 7S 6C JH 2H TH QH 3H"
    
    val (player1, player2) = twoPlayerCards.split(" ").splitAt(5)
    println (s"Player1. Input:${player1.mkString(",")} => ${generateListOfCards(player1).mkString(":")}")
    
    val dealer = new Dealer()
    
    val leftHand = generateListOfCards(player1)
    val rightHand = generateListOfCards(player2)
    
    val fullHouseStrategy = new FullHouse()
    assertTrue(fullHouseStrategy.canHandle(generateListOfCards(player1)))
    assertFalse(fullHouseStrategy.canHandle(generateListOfCards(player2)))
    
    assertEquals("left", dealer.findWinner(leftHand, rightHand))
    
  }
  
  @Test  @Ignore
  def testFourOfAKind() {
    val twoPlayerCards ="6D 7H 7J 7S 6C JH JC JD JS 3H"
    
    val (player1, player2) = twoPlayerCards.split(" ").splitAt(5)
    println (s"Player1. Input:${player1.mkString(",")} => ${generateListOfCards(player1).mkString(":")}")
    
    val dealer = new Dealer()
    
    val leftHand = generateListOfCards(player1)
    val rightHand = generateListOfCards(player2)
    
    val fourOfAKind = new FourOfAKind()
    assertFalse(fourOfAKind.canHandle(generateListOfCards(player1)))
    assertTrue(fourOfAKind.canHandle(generateListOfCards(player2)))
    
    assertEquals("right", dealer.findWinner(leftHand, rightHand))
    
  }
  
  @Test @Ignore
  def testStraightFlush() {
    val twoPlayerCards ="6D 7D 8D 9D TD JH JC JD JS 3H"
    
    val (player1, player2) = twoPlayerCards.split(" ").splitAt(5)
    println (s"Player1. Input:${player1.mkString(",")} => ${generateListOfCards(player1).mkString(":")}")
    
    val dealer = new Dealer()
    
    val leftHand = generateListOfCards(player1)
    val rightHand = generateListOfCards(player2)
    
    val straightFlush = new StraightFlush()
    assertTrue(straightFlush.canHandle(generateListOfCards(player1)))
    assertFalse(straightFlush.canHandle(generateListOfCards(player2)))
    
    assertEquals("left", dealer.findWinner(leftHand, rightHand))
    
  }
  
  @Test @Ignore
  def testRoyalFlush() {
    val twoPlayerCards ="6D 7D 8D 9D TD TH JH QH KH AH"
    
    val (player1, player2) = twoPlayerCards.split(" ").splitAt(5)
    println (s"Player1. Input:${player1.mkString(",")} => ${generateListOfCards(player1).mkString(":")}")
    
    val dealer = new Dealer()
    
    val leftHand = generateListOfCards(player1)
    val rightHand = generateListOfCards(player2)
    
    val royalFlush = new RoyalFlush()
    assertFalse(royalFlush.canHandle(generateListOfCards(player1)))
    assertTrue(royalFlush.canHandle(generateListOfCards(player2)))
    
    assertEquals("right", dealer.findWinner(leftHand, rightHand))
    
  }
  
  @Test 
  def testCodeEvalUseCases() {
    
    val usecases = List(
        "6D 7H AH 7S QC 6H 2D TD JD AS",
        "JH 5D 7H TC JS JD JC TS 5S 7S",
        "2H 8C AD TH 6H QD KD 9H 6S 6C",
        "JS JH 4H 2C 9H QH KC 9D 4D 3S",
        "TC 7H KH 4H JC 7D 9S 3H QS 7S")
        
    val results = List("left",
                       "none",
                       "right",
                       "left",
                       "right")    
        
    val dealer = new Dealer()
        
    
    for (idx <- 1 until usecases.size) {
      println(s"Usecase $idx")
      val (player1, player2) = usecases(idx).split(" ").splitAt(5)
      val leftHand = generateListOfCards(player1)
      val rightHand = generateListOfCards(player2)
    
      val result = dealer.findWinner(leftHand, rightHand)
      val expected = results(idx)
      println(s"Expected:$expected but was:$result")
      assertEquals(expected, result ) 
      println(s"Success for Usecase: $idx")
    }
                       
    
    
      
  }
  
  
}