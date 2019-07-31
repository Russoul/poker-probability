import java.util.Scanner

import cats.Show

import scala.util.Random
import cats.implicits._
import cats.instances._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

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

  implicit class Pair2List[A](lhs : (A,A)){
    def toList : List[A] = List(lhs._1, lhs._2)
  }

  implicit def showTuple3[A : Show, B : Show, C : Show] : Show[(A, B, C)] = {
    case (a, b, c) => "( " + implicitly[Show[A]].show(a) + ", " + implicitly[Show[B]].show(b) + ", " + implicitly[Show[C]].show(c) + ")"
  }

  implicit class Tuple2Util[A, B](t : (A, B)){
    def fstUpdated[R](f : A => R) : (R, B) = (f(t._1), t._2)
    def sndUpdated[R](f : B => R) : (A, R) = (t._1, f(t._2))
  }


  def factorial(k : Int) : Long = if(k >= 2) (2 to k).product else 1

  //n >= k
  def binom(n : Int, k : Int) : Long ={
    (n.toLong to (n.toLong - k.toLong + 1) by -1).product / factorial(k)
  }


  def parseSuit(str : String) : Option[Suit] = {
    str match {
      case "♠" | "S" | "s" => Some(Spades)
      case "♥" | "H" | "h" => Some(Hearts)
      case "♦" | "D" | "d" => Some(Diamonds)
      case "♣" | "C" | "c" => Some(Clubs)
      case _ => None
    }
  }

  def parseValue(str : String) : Option[(Value, String)] = {
    str match{
      case x if x.startsWith("2") => Some(Two, str.substring(1))
      case x if x.startsWith("3") => Some(Three, str.substring(1))
      case x if x.startsWith("4") => Some(Four, str.substring(1))
      case x if x.startsWith("5") => Some(Five, str.substring(1))
      case x if x.startsWith("6") => Some(Six, str.substring(1))
      case x if x.startsWith("7") => Some(Seven, str.substring(1))
      case x if x.startsWith("8") => Some(Eight, str.substring(1))
      case x if x.startsWith("9") => Some(Nine, str.substring(1))
      case x if x.startsWith("10") => Some(Ten, str.substring(2))
      case x if x.startsWith("J") | x.startsWith("j") => Some(Jack, str.substring(1))
      case x if x.startsWith("Q") | x.startsWith("q") => Some(Queen, str.substring(1))
      case x if x.startsWith("K") | x.startsWith("k") => Some(King, str.substring(1))
      case x if x.startsWith("A") | x.startsWith("a") => Some(Ace, str.substring(1))
      case _ => None
    }
  }


  def parseCardTuple(str : String) : Option[List[Card]] = {
    val regex = """ *\( *(.*) *\) *""".r
    str match{
      case regex(inner) =>
        if(inner == "") Some(Nil)
        else{
          val regex = """([^,]+)""".r
          regex.findAllIn(inner).toList.map(x => parseCard(x.trim)).sequence
        }
      case _ => None
    }
  }


  def parseCard(str : String) : Option[Card] ={
    for(v <- parseValue(str); s <- parseSuit(v._2)) yield Card(s, v._1)
  }

  implicit class CardInterpolator(str : StringContext){
    def c(any: Any*) : Card = parseCard(str.parts(0)).get
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

  def findAllPair(cards : List[Card]) : List[List[Card]] = {
    findPairsUnique(cards).map(_.toList)
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
    findAllThreeOfAKind(cards).nonEmpty
  }

  def findAllThreeOfAKind(cards : List[Card]) : List[List[Card]] = {
    val allPairs = findPairsUniqueWithEachCard(cards)
    allPairs.zip(cards).map{case (pairs, card) => card :: pairs.map(_._2)}.filter(_.size >= 3)
  }

  //checked
  def hasFourOfAKind(cards : List[Card]) : Boolean = {
    findAllFourOfAKind(cards).nonEmpty
  }

  def findAllFourOfAKind(cards : List[Card]) : List[List[Card]] = {
    val allPairs = findPairsUniqueWithEachCard(cards)
    allPairs.zip(cards).map{case (pairs, card) => card :: pairs.map(_._2)}.filter(_.size >= 4)
  }


  def tailOption[A](l : List[A]) : Option[List[A]] = {
    l match{
      case _ :: xs => Some(xs)
      case _ => None
    }
  }

  implicit class ListUtils[A](lhs : List[A]){
    def tailOption : Option[List[A]] = PokerCalc.tailOption(lhs)
  }

  //tested
  def hasStraight(cards : List[Card]) : Boolean = {
    findAllStraight(cards).nonEmpty
  }

  def findAllStraight(cards : List[Card]) : List[List[Card]] = {

    //checks if a straight can be formed with given card, thought of a central card in possible straight, and a list of other cards (it is formed heading in both directions from the given card)
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

    cards.filter(c => !List(King, Ace, Two).contains(c.value) /*King, Ace and 2 cannot be central cards in a straight(Ace cannot play a role of '1' and Ace at the same time), e.g. JQKA2, QKA23, KA234 are not straights*/).map(c => f(List(c), cards.filterNot(c == _))).map { case None => Nil; case Some(l) => l }.filter(_.nonEmpty)//try to form a straight for each card(that card being a center for a straight)
  }

  //no assumptions on cards
  def hasFlush(cards : List[Card]) : Boolean = {
    findAllFlush(cards).nonEmpty
  }

  //no assumptions on cards
  def findAllFlush(cards : List[Card]) : List[List[Card]] = {
    suits.map(suit => cards.filter(_.suit == suit)).filter(_.size >= 5)
  }


  //no assumptions on cards
  def hasFullHouse(cards : List[Card]) : Boolean = {
    findAllFullHouse(cards).nonEmpty
  }

  def findAllFullHouse(cards : List[Card]) : List[List[Card]] = {
    findAllThreeOfAKind(cards).flatMap(three => findAllPair(cards.filter(card => !three.contains(card))).map(_ ++ three))
  }

  //no assumptions on cards
  def hasTwoPairs(cards : List[Card]) : Boolean = {
    findAllPair(cards).nonEmpty
  }

  def findAllTwoPairs(cards : List[Card]) : List[List[Card]] = {
    findAllPair(cards).flatMap(two => findAllPair(cards.filter(card => !two.contains(card))).map(_ ++ two)  )
  }

  //no assumptions on cards
  def hasStraightFlush(cards : List[Card]) : Boolean = {
    findAllStraightFlush(cards).nonEmpty
  }

  def findAllStraightFlush(cards : List[Card]) : List[List[Card]] = {
    findAllStraight(cards).flatMap(straight => findAllFlush(straight))
  }


  //no assumptions on cards
  def hasRoyalFlush(cards : List[Card]) : Boolean = {
    findAllRoyalFlush(cards).nonEmpty
  }

  def findAllRoyalFlush(cards : List[Card]) : List[List[Card]] = {
    findAllFlush(cards).flatMap(flush => findAllStraight(flush).filter(_.apply(2).value == Queen))
  }

  //just puts each card in its own list
  def findAllHighHand(cards : List[Card]) : List[List[Card]] = {
    cards.map(List(_))
  }

  def bestComboHighHand(winner : (List[List[Card]], List[Card]), looser : (List[List[Card]], List[Card])) : Option[Boolean] ={
    val positive = valueToInt(winner._1.flatten.maxBy(_.value).value) - valueToInt(looser._1.flatten.maxBy(_.value).value)
    if(positive > 0) Some(true)
    else if(positive < 0) Some(false)
    else None
  }

  def otherCards(combo : List[Card], all : List[Card]) : (List[List[Card]], List[Card]) = {
    List(all.filter(c => !combo.contains(c))) -> all
  }

  def bestComboPair(winner : (List[List[Card]], List[Card]), looser : (List[List[Card]], List[Card])) : Option[Boolean] ={
    val bestWinner = winner._1.tail.foldLeft(winner._1.head.sortBy(_.value).reverse){ //code duplication
      case (a, b) =>
        val A = a.sortBy(_.value).reverse
        val B = b.sortBy(_.value).reverse
        graterThan(A, B) match{
          case Some(true) => A
          case Some(false) => B
          case None => A
        }
    }

    val bestLooser = looser._1.tail.foldLeft(looser._1.head.sortBy(_.value).reverse){ //code duplication
      case (a, b) =>
        val A = a.sortBy(_.value).reverse
        val B = b.sortBy(_.value).reverse
        graterThan(A, B) match{
          case Some(true) => A
          case Some(false) => B
          case None => A
        }
    }

    graterThan(bestWinner, bestLooser).orElse(bestComboHighHand(otherCards(bestWinner, winner._2), otherCards(bestLooser, looser._2)))
  }

  def bestComboTwoPairs(winner : (List[List[Card]], List[Card]), looser : (List[List[Card]], List[Card])) : Option[Boolean] ={
    val bestWinner = winner._1.tail.foldLeft(winner._1.head.sortBy(_.value).reverse){ //code duplication
      case (a, b) =>
        val A = a.sortBy(_.value).reverse
        val B = b.sortBy(_.value).reverse
        graterThan(A, B) match{
          case Some(true) => A
          case Some(false) => B
          case None => A
        }
    }

    val bestLooser = looser._1.tail.foldLeft(looser._1.head.sortBy(_.value).reverse){ //code duplication
      case (a, b) =>
        val A = a.sortBy(_.value).reverse
        val B = b.sortBy(_.value).reverse
        graterThan(A, B) match{
          case Some(true) => A
          case Some(false) => B
          case None => A
        }
    }

    graterThan(bestWinner, bestLooser).orElse(bestComboHighHand(otherCards(bestWinner, winner._2), otherCards(bestLooser, looser._2)))
  }

  def bestComboThreeOfAKind(winner : (List[List[Card]], List[Card]), looser : (List[List[Card]], List[Card])) : Option[Boolean] ={
    val bestWinner = winner._1.tail.foldLeft(winner._1.head.sortBy(_.value).reverse){ //code duplication
      case (a, b) =>
        val A = a.sortBy(_.value).reverse
        val B = b.sortBy(_.value).reverse
        graterThan(A, B) match{
          case Some(true) => A
          case Some(false) => B
          case None => A
        }
    }

    val bestLooser = looser._1.tail.foldLeft(looser._1.head.sortBy(_.value).reverse){ //code duplication
      case (a, b) =>
        val A = a.sortBy(_.value).reverse
        val B = b.sortBy(_.value).reverse
        graterThan(A, B) match{
          case Some(true) => A
          case Some(false) => B
          case None => A
        }
    }

    graterThan(bestWinner, bestLooser).orElse(bestComboHighHand(otherCards(bestWinner, winner._2), otherCards(bestLooser, looser._2)))
  }

  def graterThan(a : List[Card], b : List[Card]) : Option[Boolean] = {
    import valueOrdering._
    (a, b) match{
      case (a :: as, b :: bs) =>
        if(a.value > b.value)
          Some(true)
        else if(a.value < b.value)
          Some(false)
        else
          graterThan(as, bs)
      case _ => None
    }
  }

  def bestComboFlush(winner : (List[List[Card]], List[Card]), looser : (List[List[Card]], List[Card])) : Option[Boolean] ={
    val bestWinner = winner._1.tail.foldLeft(winner._1.head.sortBy(_.value).reverse){ //code duplication
      case (a, b) =>
        val A = a.sortBy(_.value).reverse
        val B = b.sortBy(_.value).reverse
        graterThan(A, B) match{
          case Some(true) => A
          case Some(false) => B
          case None => A
        }
    }

    val bestLooser = looser._1.tail.foldLeft(looser._1.head.sortBy(_.value).reverse){ //code duplication
      case (a, b) =>
        val A = a.sortBy(_.value).reverse
        val B = b.sortBy(_.value).reverse
        graterThan(A, B) match{
          case Some(true) => A
          case Some(false) => B
          case None => A
        }
    }

    //println(s"bestWinner ${bestWinner.show}")
    //println(s"bestLooser ${bestLooser.show}")

    graterThan(bestWinner, bestLooser) //no kicker
  }

  def bestComboStraight(winner : (List[List[Card]], List[Card]), looser : (List[List[Card]], List[Card])) : Option[Boolean] ={
    bestComboHighHand(winner, looser) //no kicker
  }

  def bestComboStraightFlush(winner : (List[List[Card]], List[Card]), looser : (List[List[Card]], List[Card])) : Option[Boolean] ={
    bestComboHighHand(winner, looser) //no kicker
  }

  def bestComboFullHouse(winner : (List[List[Card]], List[Card]), looser : (List[List[Card]], List[Card])) : Option[Boolean] ={
    val bestWinner = winner._1.tail.foldLeft(winner._1.head.sortBy(_.value).reverse){ //code duplication
      case (a, b) =>
        val A = a.sortBy(_.value).reverse
        val B = b.sortBy(_.value).reverse
        graterThan(A, B) match{
          case Some(true) => A
          case Some(false) => B
          case None => A
        }
    }

    val bestLooser = looser._1.tail.foldLeft(looser._1.head.sortBy(_.value).reverse){ //code duplication
      case (a, b) =>
        val A = a.sortBy(_.value).reverse
        val B = b.sortBy(_.value).reverse
        graterThan(A, B) match{
          case Some(true) => A
          case Some(false) => B
          case None => A
        }
    }

    graterThan(bestWinner, bestLooser) //no kicker
  }

  def bestComboFourOfAKind(winner : (List[List[Card]], List[Card]), looser : (List[List[Card]], List[Card])) : Option[Boolean] ={
    val bestWinner = winner._1.tail.foldLeft(winner._1.head.sortBy(_.value).reverse){ //code duplication
      case (a, b) =>
        val A = a.sortBy(_.value).reverse
        val B = b.sortBy(_.value).reverse
        graterThan(A, B) match{
          case Some(true) => A
          case Some(false) => B
          case None => A
        }
    }

    val bestLooser = looser._1.tail.foldLeft(looser._1.head.sortBy(_.value).reverse){ //code duplication
      case (a, b) =>
        val A = a.sortBy(_.value).reverse
        val B = b.sortBy(_.value).reverse
        graterThan(A, B) match{
          case Some(true) => A
          case Some(false) => B
          case None => A
        }
    }

    graterThan(bestWinner, bestLooser).orElse(bestComboHighHand(otherCards(bestWinner, winner._2), otherCards(bestLooser, looser._2)))
  }

  def bestComboRoyalFlush(winner : (List[List[Card]], List[Card]), looser : (List[List[Card]], List[Card])) : Option[Boolean] ={
    None
  }

  type ComboTy = List[Card] => List[List[Card]]
  type BestComboTy = (List[List[Card]], List[List[Card]]) => Option[Boolean]

  val scoredCombos: Map[Int, List[Card] => List[List[Card]]] = Map(
    10 -> findAllRoyalFlush _,
    9 -> findAllStraightFlush _,
    8 -> findAllFourOfAKind _,
    7 -> findAllFullHouse _,
    6 -> findAllFlush _,
    5 -> findAllStraight _,
    4 -> findAllThreeOfAKind _,
    3 -> findAllTwoPairs _,
    2 -> findAllPair _,
    1 -> findAllHighHand _
  )

  //finds (if possible) best combo if types of the combos are the same
  val findWinnerSameCombo: Map[Int, ((List[List[Card]], List[Card]), (List[List[Card]], List[Card])) => Option[Boolean]] = Map(
    10 -> bestComboRoyalFlush _,
    9 -> bestComboStraightFlush _,
    8 -> bestComboFourOfAKind _,
    7 -> bestComboFullHouse _,
    6 -> bestComboFlush _,
    5 -> bestComboStraight _,
    4 -> bestComboThreeOfAKind _,
    3 -> bestComboTwoPairs _,
    2 -> bestComboPair _,
    1 -> bestComboHighHand _
  )

  val hasCombo: Map[String, List[Card] => Boolean] = Map(
    "pair" -> hasPair _,
    "two_pairs" -> hasTwoPairs _,
    "set" -> hasThreeOfAKind _,
    "straight" -> hasStraight _,
    "flush" -> hasFlush _,
    "full_house" -> hasFullHouse _,
    "quads" -> hasFourOfAKind _,
    "straight_flush" -> hasStraightFlush _,
    "royal_flush" -> hasRoyalFlush _
  )

  def findMaxCombo(cards : List[Card]) : (Int, List[List[Card]]) = {
    scoredCombos.toStream.sortBy(_._1).reverse.foldLeft((0, Nil) : (Int, List[List[Card]])){
      case (max, (score, f)) =>
        if(max._1 != 0)
          max
        else{
          val t = f(cards)
          if(t.nonEmpty)
            score -> t
          else
            max
        }
    }
  }

  //Some(true) if 'winner' wins, Some(false) if 'looser' wins :P , None if draw
  def wins(winner : List[Card], looser : List[Card]) : Option[Boolean] = {
    val maxWinner = findMaxCombo(winner)
    val maxLooser = findMaxCombo(looser)
    //println(s"winner ${maxWinner._1}, looser ${maxLooser._1}")
    if(maxWinner._1 > maxLooser._1) Some(true)
    else if(maxWinner._1 < maxLooser._1) Some(false)
    else{ //same combo
      //println(maxWinner._2.show)
      //println(maxLooser._2.show)
      findWinnerSameCombo(maxWinner._1)(maxWinner._2 -> winner, maxLooser._2 -> looser)
    }
  }

  //all arguments may be underfilled
  def wins(communityCards : List[Card], playerCards : List[Card], opponentsCards : List[List[Card]], allowDraw : Boolean) : Boolean = {
    val allKnownCards = communityCards ++ playerCards ++ opponentsCards.flatten
    val deck0 = fullDeck.filterNot(allKnownCards.contains(_))
    val (fillCom, deck1) = chooseRandom(deck0, 5 - communityCards.size)
    val com = fillCom ++ communityCards
    val (fillPlayer, deck2) = chooseRandom(deck1, 2 - playerCards.size)
    val player = fillPlayer ++ playerCards
    val (opps, deck3) = opponentsCards.foldLeft( (Nil : List[List[Card]], deck2) ){
      case ((opps, deck1), op) =>
        val (fillOp, deck2) = chooseRandom(deck1, 2 - op.size)
        opps ++ List(fillOp ++ op) -> deck2
    }
    opps.map(opp => wins(player ++ com, opp ++ com)).forall(_.getOrElse(allowDraw))
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  def simulate(communityCards : List[Card], playerCards : List[Card], opponentsCards : List[List[Card]], allowDraw : Boolean, n : Int) : Double = {
    val threads = 8
    var win = 0

    val features = (0 until threads).map{i =>
      Future{
        var win = 0
        for(_ <- n/threads * i until n/threads * (i + 1)){
          if(wins(communityCards, playerCards, opponentsCards, allowDraw)) win += 1
        }

        win
      }
    }

    for(l <- Await.result(Future.sequence(features), Duration("Inf")).toList){
      win += l
    }

    win.toDouble / n
  }

  def simulateCombo(communityCards : List[Card], playerCards : List[Card], opponentsCards : List[List[Card]], hasCombo : List[Card] => Boolean, within : Int,  n : Int) : Double = {
    val threads = 8
    var win = 0

    val deck = fullDeck.filterNot(x => communityCards.contains(x) || playerCards.contains(x) || opponentsCards.flatten.contains(x))

    val features = (0 until threads).map{i =>
      Future{
        var win = 0
        for(_ <- n/threads * i until n/threads * (i + 1)){
          if(hasCombo(playerCards ++ communityCards ++ chooseRandom(deck, within)._1)) win += 1
        }

        win
      }
    }

    for(l <- Await.result(Future.sequence(features), Duration("Inf")).toList){
      win += l
    }

    win.toDouble / n
  }

  def simulate(communityCards : List[Card], playerCards : List[Card], opponentsCards : List[List[Card]], allowDraw : Boolean, n : Int, k : Int) : (Double, Double) = {
    val simulations = new Array[Double](k)
    for (i <- 0 until k){
      simulations(i) = simulate(communityCards, playerCards, opponentsCards, allowDraw, n)
    }

    val mean = simulations.sum / k
    var v = 0.0
    for (i <- 0 until k){
      v += (simulations(i) - mean) * (simulations(i) - mean)
    }
    mean -> math.sqrt(v)
  }

  def simulateCombo(communityCards : List[Card], playerCards : List[Card], opponentsCards : List[List[Card]], hasCombo : List[Card] => Boolean, within : Int,  n : Int, k : Int) : (Double, Double) = {
    val simulations = new Array[Double](k)
    for (i <- 0 until k){
      simulations(i) = simulateCombo(communityCards, playerCards, opponentsCards, hasCombo, within, n)
    }

    val mean = simulations.sum / k
    var v = 0.0
    for (i <- 0 until k){
      v += (simulations(i) - mean) * (simulations(i) - mean)
    }
    mean -> math.sqrt(v)
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

  def probWithinNext7StraightFullDeck : Double = {
    val numberOfStraightVariants = 4 * 4 * 4 * 4 * 4 * 13
    (binom(47, 2) * numberOfStraightVariants - 23 * numberOfStraightVariants * 46 - 186 * numberOfStraightVariants).toDouble / binom(52, 7)
  }

  //analytical solution to the general case ^^^ when there are some playing cards is painful to derive,
  //because the prob depends heavily on known playing cards
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

  def chooseRandom[A](objs : List[A], n : Int) : (List[A], List[A]) = {
    if(n > objs.size) throw new Exception

    if(n == 0) Nil -> objs
    else{
      val (a, as) = chooseRandom(objs)
      chooseRandom(as, n - 1).fstUpdated(a :: _)
    }

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

  var allowDraw : Boolean = true
  var n : Int = 100000
  var k : Int = 10

  var player : List[Card] = Nil
  var community : List[Card] = Nil
  var otherPlayers : List[List[Card]] = List(Nil)

  def processCommand(cmd : String) : Unit = {
    println("-------------")
    val cardsReg = " *cards +( *\\([^\\)]*\\) *) +( *\\([^\\)]*\\) *) +(.*)".r
    val allowDrawReg = " *draw (.*)".r
    val nReg = " *n (.*)".r
    val kReg = " *k (.*)".r
    val simReg = " *sim".r
    val simReg2 = " *sim +mean".r
    val hasReg = " *(\\w+) +(\\d+)".r
    val hasReg2 = " *(\\w+) +(\\d+) +mean".r
    cmd match{
      case cardsReg(com, player, others) =>
        val comL = parseCardTuple(com)
        val playerL = parseCardTuple(player)
        comL match{
          case None => println("failed parsing community cards in sim")
          case Some(comL) =>
            playerL match{
              case None => println("failed parsing player cards in sim")
              case Some(playerL) =>
                val reg = " *\\([^\\)]*\\) *".r
                reg.findAllIn(others).toList.map(parseCardTuple(_)).sequence match{
                  case None => println("failed parsing other players in sim")
                  case Some(othersL) =>
                    println(s"community cards: ${comL.show}")
                    println(s"player cards: ${playerL.show}")
                    println(s"others cards: ${othersL.show}")
                    this.player = playerL
                    this.community = comL
                    this.otherPlayers = othersL
                }
            }
        }
      case allowDrawReg(draw) =>
        try{
          val d = draw.toBoolean
          allowDraw = d
          println("set allowDraw to '" + d + "'")
        }catch{
          case _ : IllegalArgumentException => println("failed parsing bool argument for 'draw'")
        }
      case nReg(n) =>
        try{
          val d = n.toInt
          this.n = d
          println("set n to '" + d + "'")
        }catch{
          case _ : NumberFormatException => println("failed parsing Int argument for 'n'")
        }
      case kReg(k) =>
        try{
          val d = k.toInt
          this.k = d
          println("set k to '" + d + "'")
        }catch{
          case _ : NumberFormatException => println("failed parsing Int argument for 'n'")
        }
      case simReg() =>
        println("simulating...")
        println("probability: " + simulate(community, player, otherPlayers, allowDraw, n))
      case simReg2() =>
        println("simulating mean...")
        val sim = simulate(community, player, otherPlayers, allowDraw, n, k)
        println("mean probability: " + sim._1 + ", mean deviation: " + sim._2)
      case hasReg(name, within) if hasCombo.keys.toList.contains(name) =>
        println("simulating combo...")
        val sim = simulateCombo(community, player, otherPlayers, hasCombo(name), within.toInt, n)
        println("probability: " + sim)
      case hasReg2(name, within) if hasCombo.keys.toList.contains(name) =>
        println("simulating mean combo...")
        val sim = simulateCombo(community, player, otherPlayers, hasCombo(name), within.toInt, n, k)
        println("mean probability: " + sim._1 + ", mean deviation: " + sim._2)
      case _ => println("bad command")
    }
    println("-------------")
  }

  def loop() : Unit = {
    println("Entering loop")
    val s = new Scanner(System.in)
    var shouldStop = false
    while (!shouldStop){
      val cmd = s.nextLine()
      if(cmd == "quit" || cmd == "exit")
        shouldStop = true
      else
        processCommand(cmd)
    }
    println("Byeee")
  }

  def main(args : Array[String]) : Unit = {
    test()

    loop()

    //println(simulate(List(c"Ad", c"As"), List(c"Ac", c"Ah"), List(Nil, Nil), allowDraw = false, 100000))
    //println(simulate(List(c"2d", c"6d", c"Ad", c"3h"), List(c"Jd", c"4d"), List(List(c"Kd", c"Qd"), Nil), allowDraw = true, 100000, 7))
    //println(simulate(Nil, List(c"Ac", c"Ah"), List(Nil, Nil, Nil, Nil), allowDraw = false, 100000))
    //println(simulate(Nil, Nil, List(Nil, Nil, Nil), allowDraw = false, 100000, 10))

   /* val hand0 = Hand(Card(Diamonds, King), Card(Hearts, Three))
    val hand1 = Hand(Card(Diamonds, King), Card(Hearts, King))
    val playing0 = List(Card(Clubs, King), Card(Diamonds, Ace), Card(Hearts, King), Card(Clubs, Two), Card(Diamonds, Four), Card(Diamonds, Three))
    val playing1 = List(Card(Clubs, Five), Card(Clubs, Seven), Card(Clubs, Eight), Card(Clubs, Nine), Card(Clubs, Jack), Card(Clubs, Queen))

    println(s"p8 = ${probWithinNext7StraightFullDeck} and ${testStraightProbWithinNCardsWithPlayingCards(7, 1000000, Nil)}")
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
    println(s"p4 = ${p4} and ${testThreeOfAKindProbWithinNCardsWithPlayingCards(5, 1000000, hand1.cards.toList)}")*/

  }


  def test() : Unit = {
    testHasStraight()
    testHasFullHouse()
    testHasFlush()
    testHasRoyalFlush()
    testHasFourOfAKind()
    testWins()
  }


  def testHasStraight() : Unit = {
    val bundle1 = List(
      List(Ace, Two, Three, Four, Five),
      List(Four, Five, Six, Seven, Eight),
      List(Jack, Ten, Nine, Queen, King),
      List(Four, Three, Two, Ace, Five),
      List(King, Queen, Jack, Ten, Nine),
      List(Two, Three, Four, Five, Six)
    )

    val bundle2 = List(
      List(Ace, Two, Three, Four, King),
      List(Four, Three, Two, Ace, King),
    )

    val testBundle1 = bundle1.map(_.map(x => Card(chooseRandom(suits)._1, x))).find(x => !hasStraight(x))
    val testBundle2 = bundle2.map(_.map(x => Card(chooseRandom(suits)._1, x))).find(x => hasStraight(x))

    if(testBundle1.isEmpty && testBundle2.isEmpty){
      println("testHasStraight - ok")
    }else{
      println("testHasStraight - failed")
      println("must form a straight " + testBundle1.show)
      println("must not form a straight " + testBundle2.show)
    }
  }

  def testHasFullHouse() : Unit = {
    val bundle1 = List(
      List(Card(Clubs, Ace), Card(Clubs, Two), Card(Spades, Two), Card(Diamonds, Ace), Card(Hearts, Ace)),
      List(Card(Clubs, King), Card(Spades, King), Card(Clubs, Ace), Card(Diamonds, Ace), Card(Hearts, Ace))
    )

    val bundle2 = List(
      List(Card(Spades, Ace), Card(Diamonds, Ace), Card(Clubs, Ace), Card(Hearts, Ace), Card(Hearts, King)),
      List(Card(Spades, Ace), Card(Diamonds, Ace), Card(Diamonds, Two), Card(Clubs, Two), Card(Clubs, King)),
    )

    val testBundle1 = bundle1.find(x => !hasFullHouse(x))
    val testBundle2 = bundle2.find(x => hasFullHouse(x))

    if(testBundle1.isEmpty && testBundle2.isEmpty){
      println("testHasFullHouse - ok")
    }else{
      println("testHasFullHouse - failed")
      println("must form a full house " + testBundle1.show)
      println("must not form a full house " + testBundle2.show)
    }
  }

  def testHasFlush() : Unit = {
    val bundle1 = List(
      List.fill(5)(Spades),
      List.fill(5)(Hearts),
      List.fill(5)(Diamonds),
      List.fill(5)(Clubs)
    )

    val bundle2 = List(
      Diamonds :: List.fill(4)(Spades),
      List(Spades, Hearts, Hearts, Diamonds, Diamonds, Diamonds)
    )

    val testBundle1 = bundle1.map(_.map(x => Card(x, chooseRandom(values)._1))).find(x => !hasFlush(x))
    val testBundle2 = bundle2.map(_.map(x => Card(x, chooseRandom(values)._1))).find(x => hasFlush(x))

    if(testBundle1.isEmpty && testBundle2.isEmpty){
      println("testHasFlush - ok")
    }else{
      println("testHasFlush - failed")
      println("must form a flush " + testBundle1.show)
      println("must not form a flush " + testBundle2.show)
    }
  }

  def testHasRoyalFlush() : Unit = {
    val bundle1 = List(
      List(King, Ace, Jack, Ten, Queen)
    )

    val bundle2 = List(
      List(King, Jack, Queen, Ten, Nine)
    )

    val testBundle1 = bundle1.map(_.map(x => Card(Clubs, x))).find(x => !hasRoyalFlush(x))
    val testBundle2 = bundle2.map(_.map(x => Card(Clubs, x))).find(x => hasRoyalFlush(x))

    if(testBundle1.isEmpty && testBundle2.isEmpty){
      println("testHasRoyalFlush - ok")
    }else{
      println("testHasRoyalFlush - failed")
      println("must form a royal flush " + testBundle1.show)
      println("must not form a royal flush " + testBundle2.show)
    }
  }

  def testHasFourOfAKind() : Unit = {
    val bundle1 = List(
      List(King, King, King, King, Ace, Ace, Ace, Ace),
      List(King, King, King, Ace, Two, Two, Two, Ten, Two)
    )

    val bundle2 = List(
      List(King, Ace, Two, Two, Ace, Two, Ace, King, King),
      List(Ace, Ace, Ace, King, King, King, Three, Three, Two, Two, Three)
    )

    val testBundle1 = bundle1.map(_.map(x => Card(chooseRandom(suits)._1, x))).find(x => !hasFourOfAKind(x))
    val testBundle2 = bundle2.map(_.map(x => Card(chooseRandom(suits)._1, x))).find(x => hasFourOfAKind(x))


    if(testBundle1.isEmpty && testBundle2.isEmpty){
      println("testHasFourOfAKind - ok")
    }else{
      println("testHasFourOfAKind - failed")
      println("must form a four of a kind " + testBundle1.show)
      println("must not form a four of a kind " + testBundle2.show)
    }

  }

  def testWins() : Unit = {


    //♠
    //♥
    //♦
    //♣

    val bundle1 = List(//left wins
      (List(c"K♠", c"2♠", c"6♠", c"6♣", c"4♣"), List(c"A♠", c"Q♠"), List(c"4♠", c"3♠")),
      (List(c"K♠", c"2♠", c"6♠", c"6♣", c"4♣"), List(c"6♥", c"Q♥"), List(c"2♥", c"3♠")),
      (List(c"K♠", c"2♠", c"6♠", c"6♣", c"2♦"), List(c"6♥", c"Q♥"), List(c"2♥", c"3♠")),
      (List(c"6♦", c"7♠", c"6♠", c"6♣", c"7♦"), List(c"6♥", c"Q♥"), List(c"7♥", c"3♠")),
      (List(c"6♦", c"7♠", c"8♠", c"J♣", c"A♦"), List(c"9♥", c"10♥"), List(c"5♥", c"4♠")),
    )
    val bundle2 = List(//right wins
      (List(c"K♠", c"2♠", c"6♠", c"6♣", c"4♣"), List(c"4♠", c"J♠"), List(c"A♠", c"Q♠")),
      (List(c"6♦", c"7♠", c"8♠", c"J♣", c"10♦"), List(c"3♥", c"Q♥"), List(c"K♥", c"4♠")),
      (List(c"6♦", c"7♠", c"8♠", c"J♣", c"10♦"), List(c"3♥", c"Q♥"), List(c"4♥", c"4♠")),
      (List(c"6♦", c"7♠", c"8♠", c"J♣", c"10♦"), List(c"8♥", c"Q♥"), List(c"J♥", c"4♠")),
      (List(c"6♦", c"6♠", c"4♣", c"J♣", c"10♦"), List(c"J♥", c"Q♥"), List(c"4♥", c"4♠")),
      (List(c"6♦", c"6♠", c"4♣", c"J♣", c"10♦"), List(c"4♥", c"4♠"), List(c"6♥", c"J♠")),
    )

    val bundle3 = List(//draw
      (List(c"6♦", c"7♠", c"8♠", c"A♣", c"K♦"), List(c"6♥", c"J♥"), List(c"6♥", c"5♠")),
      (List(c"6♦", c"7♠", c"8♠", c"J♣", c"A♦"), List(c"3♥", c"Q♥"), List(c"K♥", c"4♠")),

    )


    val testBundle1 = bundle1.map(b => b -> wins(b._1 ++ b._2, b._1 ++ b._3)).find(_._2.getOrElse(false) == false)
    val testBundle2 = bundle2.map(b => b -> wins(b._1 ++ b._2, b._1 ++ b._3)).find(_._2.getOrElse(true) == true)
    val testBundle3 = bundle3.map(b => b -> wins(b._1 ++ b._2, b._1 ++ b._3)).find(_._2.nonEmpty)

    if(testBundle1.isEmpty && testBundle2.isEmpty && testBundle3.isEmpty){
      println("testWins - ok")
    }else{
      println("testWins - failed")
      println("left must win " + testBundle1.map(_._1).show)
      println("left must loose " + testBundle2.map(_._1).show)
      println("draw " + testBundle3.map(_._1).show)
    }


  }

}
