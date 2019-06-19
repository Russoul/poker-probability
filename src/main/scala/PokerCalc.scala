import cats.Show

import scala.util.Random
import cats.implicits._
import cats.instances._

object PokerCalc {

  sealed trait Suit
  sealed trait Value


  object Spades extends Suit
  object Hearts extends Suit
  object Diamonds extends Suit
  object Clubs extends Suit

  object Two extends Value
  object Three extends Value
  object Four extends Value
  object Five extends Value
  object Six extends Value
  object Seven extends Value
  object Eight extends Value
  object Nine extends Value
  object Ten extends Value
  object Jack extends Value
  object Queen extends Value
  object King extends Value
  object Ace extends Value

  case class Card(suit : Suit, value : Value)
  case class Hand(cards : (Card, Card))

  implicit class Pair2List[A](lhs : (A,A)){
    def toList : List[A] = List(lhs._1, lhs._2)
  }


  def factorial(k : Int) : Long = if(k >= 2) (2 to k).product else 1

  //n >= k
  def binom(n : Int, k : Int) : Long ={
    (n.toLong to (n.toLong - k.toLong + 1) by -1).product / factorial(k)
  }



  implicit val suitIsShowable : Show[Suit] = {
    case Spades => "♠"
    case Hearts => "♥"
    case Diamonds => "♦"
    case Clubs => "♣"
  }

  implicit val valueIsShowable : Show[Value] ={
    case Two => "2"
    case Three => "3"
    case Four => "4"
    case Five => "5"
    case Six => "6"
    case Seven => "7"
    case Eight => "8"
    case Nine => "9"
    case Ten => "10"
    case Jack => "J"
    case Queen => "Q"
    case King => "K"
    case Ace => "A"
  }

  implicit val cardIsShowable : Show[Card] = {
    case Card(suit, value) => implicitly[Show[Value]].show(value) + implicitly[Show[Suit]].show(suit)
  }

  val valueCount = 13
  val suitCount = 4
  val totalCards = valueCount * suitCount //52

  val suits = List(Spades, Hearts, Diamonds, Clubs)
  val values = List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)

  val fullDeck : List[Card] = for(suit <- suits; value <- values) yield Card(suit, value)

  def sameValue(cards : List[Card]) : Boolean = {
    cards match{
      case x :: xs => xs.forall(_.value == x.value)
      case Nil => true
    }
  }

  def prev(v : Value) : Value = {
    v match{
      case Two => Ace
      case Three => Two
      case Four => Three
      case Five => Four
      case Six => Five
      case Seven => Six
      case Eight => Seven
      case Nine => Eight
      case Ten => Nine
      case Jack => Ten
      case Queen => Jack
      case King => Queen
      case Ace => King

    }
  }

  def next(v : Value) : Value = {
    v match{
      case Two => Three
      case Three => Four
      case Four => Five
      case Five => Six
      case Six => Seven
      case Seven => Eight
      case Eight => Nine
      case Nine => Ten
      case Ten => Jack
      case Jack => Queen
      case Queen => King
      case King => Ace
      case Ace => Two
    }
  }

  def valueToInt(v : Value) : Int = {
    v match{
      case Two => 2
      case Three => 3
      case Four => 4
      case Five => 5
      case Six => 6
      case Seven => 7
      case Eight => 8
      case Nine => 9
      case Ten => 10
      case Jack => 11
      case Queen => 12
      case King => 13
      case Ace => 14
    }
  }

  implicit val valueOrdering : Ordering[Value] = {
    (x: Value, y: Value) => {
      valueToInt(x) - valueToInt(y)
    }
  }

  def hasPair(cards : List[Card]) : Boolean = {
    cards.exists(x => cards.exists(y => x != y && x.value == y.value))
  }

  //returns all unique pairs
  def findPairsUnique(cards : List[Card]) : List[(Card, Card)] = {
    findPairsUniqueWithEachCard(cards).flatten
  }

  //returns all unique pairs for each card
  def findPairsUniqueWithEachCard(cards : List[Card]) : List[List[(Card, Card)]] = {
    cards.zipWithIndex.map{case (card, i) => (card, cards.drop(i + 1))}.map{case (x,cards) => cards.filter(y => x.value == y.value).map(y => (x, y))}
  }

  //for each card returns all pairs with that card
  def findPairsWithEachCard(cards : List[Card]) : List[List[(Card, Card)]] = {
    cards.map{x => cards.filter(y => x != y && x.value == y.value).map(y => (x, y))}
  }

  //checked
  def hasThreeOfAKind(cards : List[Card]) : Boolean = {
    val allPairs = findPairsWithEachCard(cards)
    allPairs.exists(_.size >= 2)
  }

  //checked
  def hasFourOfAKind(cards : List[Card]) : Boolean = {
    val allPairs = findPairsWithEachCard(cards)
    allPairs.exists(_.size == 3)
  }


  def tailOption[A](l : List[A]) : Option[List[A]] = {
    l match{
      case x :: xs => Some(xs)
      case _ => None
    }
  }

  implicit class ListUtils[A](lhs : List[A]){
    def tailOption : Option[List[A]] = PokerCalc.tailOption(lhs)
  }

  //tested
  def hasStraight(cards : List[Card]) : Boolean = {

    def f(combo : List[Card], cards : List[Card]) : Option[List[Card]] = {

      if(combo.size >= 5) Some(combo)
      else{
        val min = combo.head
        val max = combo.last
        val minPrev = prev(min.value)
        val maxNext = next(max.value)
        val (mins, othersMin) = cards.partition(_.value == minPrev)
        val (maxs, others) = othersMin.partition(_.value == maxNext)
        val minMax = for(min <- mins.headOption.map(List(_)); max <- maxs.headOption.map(List(_))) yield min ++ max
        minMax.flatMap(x => f(x.head :: (combo ++ List(x.last)), others))
      }
    }

    cards.map(c => f(List(c), cards.filterNot(c == _))).exists(_.nonEmpty)
  }

  //returns pairs with unique elements(so hasThreeOfAKind => hasTwoPairs is not true for all inputs)
  //checked
  def hasTwoPairs(cards : List[Card]) : Boolean = {

    def _hasTwoPairs(cards : List[Card], i : Int = 0) : Boolean = {
      if(i == 2)
        true
      else{
        cards match{
          case c :: cs =>
            val (same, notSame) = cs.partition(cc => cc.value == c.value)
            same match{
              case _ :: sameOther =>
                _hasTwoPairs(sameOther ++ notSame, i + 1)
              case Nil => _hasTwoPairs(notSame, i)
            }
          case Nil => false
        }
      }

    }

    _hasTwoPairs(cards)
  }

  //prob of another pair when next card is drawn from the deck, assuming playing cards do not contain any combo
  //checked
  def probNextPair(playing : Int) : Double = {
    playing * 3D / (52 - playing)
  }



  //prob of another pair but not set when next card is drawn from the deck, assuming playing cards do not contain any combo greater than two pairs
  //checked
  def probNextPairButNotThreeOfAKind(playing : Int, pairs : Int) : Double = {
    val nonPairs = playing - pairs * 2
    (nonPairs * 3D) / (52 - playing)
  }

  //prob of another pair but not quad when next card is drawn from the deck, assuming playing cards do not contain any combo greater than set
  //checked
  def probNextPairButNotFourOfAKind(playing : Int, sets : Int) : Double = {
    val nonPairs = playing - sets * 3
    (nonPairs * 3D) / (52 - playing)
  }

  //prob of pair when next n cards are drawn from the deck, assuming playing cards do not contain any combo greater than pair
  //checked
  def probWithinNextPair(n : Int, playing : Int) : Double = {
    if(n == 1) probNextPair(playing)
    else {
      val probNext = probNextPair(playing)
      probNext + (1 - probNext) * probWithinNextPair(n - 1, playing + 1)
    }
  }

  //prob of a set when next card is drawn from the deck, assuming playing cards do not contain any combo greater than pair but contain at least one pair
  //checked
  def probNextThreeOfAKind(playing : Int, pairs : Int) : Double = {
    pairs * 2D / (52 - playing)
  }

  //prob of a quad when next card is drawn from the deck, assuming playing cards do not contain any combo greater than set but contain at least one set
  //checked
  def probNextFourOfAKind(playing : Int, sets : Int) : Double = {
    sets * 1D / (52 - playing)
  }


  //prob of two pairs when next card is drawn from deck, assuming playing cards do not contain any combo greater than set but contain a pair xor a set
  //checked
  def probNextTwoPairs(playing : Int, pair : Int, set : Int) : Double = {
    val single = playing - pair * 2 - set * 3
    (single * 3D + set * 1D) / (52 - playing)
  }

  //prob of two pairs when next n cards are drawn from the deck, assuming playing cards do not contain any combo greater than set and do not contain two pairs
  //checked
  def probWithinNextTwoPairs(n : Int, playing : Int, pair : Int, set : Int) : Double = {
    if(n == 1){
      if(pair + set == 0) 0
      else{
        probNextTwoPairs(playing, pair, set)
      }
    }else{
      if(pair != 0 && set == 0){
        val pairNotSet = probNextPairButNotThreeOfAKind(playing, 1)
        val nextSet = probNextThreeOfAKind(playing, 1)
        pairNotSet + nextSet * probWithinNextTwoPairs(n - 1, playing + 1, 0, 1) + (1 - pairNotSet - nextSet) * probWithinNextTwoPairs(n - 1, playing + 1, 1, 0)
      }else if(set == 1){ //pair == 0
        val pairNotFour = probNextPairButNotFourOfAKind(playing, 1)
        val nextFour = probNextFourOfAKind(playing, 1)
        pairNotFour + nextFour + (1 - pairNotFour - nextFour) * probWithinNextTwoPairs(n - 1, playing + 1, 0, 1)

      }else{
        val nextPair = probNextPair(playing)
        nextPair * probWithinNextTwoPairs(n - 1, playing + 1, 1, 0) + (1 - nextPair) * probWithinNextTwoPairs(n - 1, playing + 1, 0, 0)
      }
    }
  }

  //prob of a set when next n cards are drawn from the deck, assuming playing cards do not contain a set
  //checked
  def probWithinNextThreeOfAKind(n : Int, playing : Int, pairs : Int) : Double = {
    if(pairs == 0 && n == 1) 0
    else{
      if(n == 1) probNextThreeOfAKind(playing, pairs)
      else{
        val probNext = probNextThreeOfAKind(playing, pairs)
        val probPair = probNextPairButNotThreeOfAKind(playing, pairs)
        probNext + probPair * probWithinNextThreeOfAKind(n - 1, playing + 1, pairs + 1) + (1 - probNext - probPair) * probWithinNextThreeOfAKind(n - 1, playing + 1, pairs)
      }
    }
  }

  def probWithinNextFourOfAKind(n : Int, playing : Int, pairs : Int, sets : Int) : Double = {
    if(sets == 0 && n == 1) 0
    else{
      if(n == 1) probNextFourOfAKind(playing, sets)
      else{
        val probNext = probNextFourOfAKind(playing, sets)
        val probPair = probNextPairButNotThreeOfAKind(playing - sets * 3, pairs)
        val probSet = probNextThreeOfAKind(playing - sets * 3, pairs)
        probNext + probPair * probWithinNextFourOfAKind(n - 1, playing + 1, pairs + 1, sets) + probSet * probWithinNextFourOfAKind(n - 1, playing + 1, pairs, sets + 1) + (1 - probNext - probPair - probSet) * probWithinNextFourOfAKind(n - 1, playing + 1, pairs, sets)
      }
    }
  }

  def probWithinNextStraightFullDeck(n : Int) : Double = {
    (0 until 4*4*4*4*4).map(x => {
      val z0 = x % 4
      val z1 = (x / 4) % 4
      val z2 = (x / 16) % 4
      val z3 = (x / 64) % 4
      val z4 = (x / 256) % 4
      binom(52-4-5-z0-z1-z2-z3-z4, n-5)//better way of calculating the sums
    }
    ).sum * 13D / binom(52, n)
  }

  //analytical solution to the general case ^^^ when there are some playing cards is painful to derive,
  //use the LBN but make it more robust (error within bounds with particular probability) ?


  //==================================================
  //======= TESTS USING LAW OF LARGE NUMBERS =========
  //==================================================

  //returns chosen object and all other objects
  //obj must be nonEmpty
  def chooseRandom[A](objs : List[A]) : (A, List[A]) = {
    val randIndex = new Random().nextInt(objs.size)
    def f(objs : List[A], i : Int = 0, others : List[A] = Nil) : List[A] = {
      objs match{
        case x :: xs if i != randIndex => f(xs, i + 1, x :: others)
        case x :: xs => x :: f(xs, i + 1, others)
        case Nil => others
      }
    }

    val headTail = f(objs)
    (headTail.head, headTail.tail)
  }

  def drawCardsFromShuffledDeck(n : Int, deck : List[Card]) : List[Card] = {
    if(n == 0) Nil
    else{
      val (card, newDeck) = chooseRandom(deck)
      card :: drawCardsFromShuffledDeck(n - 1, newDeck)
    }
  }

  def testTwoPairsProbNextCardWithPlayingCards(numberOfTests : Int, playing : List[Card]) : Double = {
    val deck = fullDeck.filter(x => !playing.contains(x))
    (0 until numberOfTests).map(_ => hasTwoPairs(playing ++ drawCardsFromShuffledDeck(1, deck))).count(x => x).toDouble / numberOfTests
  }

  def testPairProbWithinNCards(n : Int, numberOfTests : Int) : Double = {
    (0 until numberOfTests).map(_ => hasPair(drawCardsFromShuffledDeck(n, fullDeck))).count(x => x).toDouble / numberOfTests
  }

  def testThreeOfAKindProbWithinNCards(n : Int, numberOfTests : Int) : Double = {
    (0 until numberOfTests).map(_ => hasThreeOfAKind(drawCardsFromShuffledDeck(n, fullDeck))).count(x => x).toDouble / numberOfTests
  }

  def testFourOfAKindProbWithinNCards(n : Int, numberOfTests : Int) : Double = {
    (0 until numberOfTests).map(_ => hasFourOfAKind(drawCardsFromShuffledDeck(n, fullDeck))).count(x => x).toDouble / numberOfTests
  }

  def testTwoPairsProbWithinNCards(n : Int, numberOfTests : Int) : Double = {
    (0 until numberOfTests).map(_ => hasTwoPairs(drawCardsFromShuffledDeck(n, fullDeck))).count(x => x).toDouble / numberOfTests
  }

  def testThreeOfAKindProbWithinNCardsWithPlayingCards(n : Int, numberOfTests : Int, playing : List[Card]) : Double = {
    val deck = fullDeck.filter(x => !playing.contains(x))
    (0 until numberOfTests).map(_ => hasThreeOfAKind(playing ++ drawCardsFromShuffledDeck(n, deck))).count(x => x).toDouble / numberOfTests
  }

  def testFourOfAKindProbWithinNCardsWithPlayingCards(n : Int, numberOfTests : Int, playing : List[Card]) : Double = {
    val deck = fullDeck.filter(x => !playing.contains(x))
    (0 until numberOfTests).map(_ => hasFourOfAKind(playing ++ drawCardsFromShuffledDeck(n, deck))).count(x => x).toDouble / numberOfTests
  }

  def testStraightProbWithinNCardsWithPlayingCards(n : Int, numberOfTests : Int, playing : List[Card]) : Double = {
    val deck = fullDeck.filter(x => !playing.contains(x))
    (0 until numberOfTests).map(_ => hasStraight(playing ++ drawCardsFromShuffledDeck(n, deck))).count(x => x).toDouble / numberOfTests
  }


  //assuming playing cards do not contain a pair
  def testPairProbWithinNCardsWithPlayingCards(n : Int, numberOfTests : Int, playing : List[Card]) : Double = {
    val deck = fullDeck.filter(x => !playing.contains(x))
    (0 until numberOfTests).map(_ => hasPair(playing ++ drawCardsFromShuffledDeck(n, deck))).count(x => x).toDouble / numberOfTests
  }


  //=========================================
  //=========================================
  //=========================================

  def main(args : Array[String]) : Unit = {
    val hand0 = Hand(Card(Diamonds, King), Card(Hearts, Three))
    val hand1 = Hand(Card(Diamonds, King), Card(Hearts, King))
    val playing0 = List(Card(Clubs, King), Card(Diamonds, Ace), Card(Hearts, King), Card(Clubs, Two), Card(Diamonds, Four), Card(Diamonds, Three))
    val playing1 = List(Card(Clubs, Five), Card(Clubs, Seven), Card(Clubs, Eight), Card(Clubs, Nine), Card(Clubs, Jack), Card(Clubs, Queen))
    //testBundle()
    println(s"p8 = ${probWithinNextStraightFullDeck(5)} and ${testStraightProbWithinNCardsWithPlayingCards(5, 1000000, Nil)}")
    val p7 = probWithinNextFourOfAKind(5, 2, 1, 0)
    println(s"p7 = ${p7} and ${testFourOfAKindProbWithinNCardsWithPlayingCards(5, 1000000, hand1.cards.toList)}")
    val p6 = probWithinNextFourOfAKind(7, 0, 0, 0)
    println(s"p6 = ${p6} and ${testFourOfAKindProbWithinNCards(7, 1000000)}")
    val p5 = probWithinNextTwoPairs(7, 0, 0, 0)
    println(s"p5 = ${p5} and ${testTwoPairsProbWithinNCards(7, 1000000)}")
    val p1 = probNextTwoPairs(6, 1, 0)
    println(s"p1 = ${p1} and ${testTwoPairsProbNextCardWithPlayingCards(1000000, playing0)}")
    val p2 = probWithinNextPair(7, 0)
    println(s"p2 = ${p2} and ${testPairProbWithinNCards(7, 1000000)}")
    val p3 = probWithinNextPair(5, 2)
    println(s"p3 = ${p3} and ${testPairProbWithinNCardsWithPlayingCards(5, 1000000, hand0.cards.toList)}")
    val p4 = probWithinNextThreeOfAKind(5, 2, 1)
    println(s"p4 = ${p4} and ${testThreeOfAKindProbWithinNCardsWithPlayingCards(5, 1000000, hand1.cards.toList)}")

  }


  def testBundle() : Unit = {
    val bundle = List(
      List(Two, King, Ace, Three, Four),
      List(Four, Five, Six, Seven, Eight),
      List(Jack, Ten, Nine, Queen, King),
      List(Four, Three, Two, Ace, King),
      List(King, Queen, Jack, Ten, Nine),
      List(Two, Three, Four, Five, Six)
    )

    println(bundle.map(_.map(x => Card(chooseRandom(suits)._1, x))).find(x => !hasStraight(x)).show)
  }

}
