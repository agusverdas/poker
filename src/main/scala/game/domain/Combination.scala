package edu.agus.poker
package game.domain

sealed trait Combination {
  def name: String

  def testCase: TestCase

  def combination: List[Card]
}

sealed abstract case class HighCard(testCase: TestCase) extends Combination {
  override val name: String = HighCard.name
}

object HighCard {
  val name = "High Card"

  def evaluate(list: List[Card]): List[Card] = list.sortBy(_.rank.order).takeRight(5)

  def evaluateKicker(list: List[Card]): Card = list.maxBy(_.rank.order)

  def apply(testCase: TestCase): Combination = {
    val allCards = testCase.board.value ::: testCase.hand.value
    new HighCard(testCase) {
      override val combination: List[Card] = evaluate(allCards)
    }
  }
}

sealed abstract case class Pair private(testCase: TestCase) extends Combination {
  override val name: String = Pair.name
}

object Pair {
  val name = "Pair"
  def evaluate(list: List[Card]): Option[List[Card]] = {
    val handState = HandState.evaluateState(list)
    handState.find {
      case (_, listCards) => listCards.length == 2
    }.map {
      case (rank, pair) =>
        val others = handState.filter {
          case (entryRank, _) => entryRank != rank
        }
        val sortedOthers = others.values.toList.flatten.sortBy(_.rank.order)
        sortedOthers.take(3) ::: pair
    }
  }

  def apply(testCase: TestCase): Option[Combination] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val pair = evaluate(allCards)
    Option.when(pair.isDefined)(new Pair(testCase) {
      override val combination: List[Card] = pair.get
    })
  }
}

sealed abstract case class TwoPairs private(testCase: TestCase) extends Combination {
  override val name: String = TwoPairs.name
}

object TwoPairs {
  val name = "Two Pairs"

  def evaluate(list: List[Card]): Option[List[Card]] = {
    val handState = HandState.evaluateState(list)
    val (pairs, pairsRanks, others) = handState.foldLeft((List.empty[List[Card]], List.empty[String], List.empty[Card])) {
      case ((pairs, ranks, others), (itRank, cards)) =>
        if (cards.size == 2) {
          (cards :: pairs, itRank :: ranks, others)
        } else {
          (pairs, ranks, cards ::: others)
        }
    }
    if (pairs.size < 2) None
    else if (pairs.size == 2) {
      val sortedOthers = others.sortBy(_.rank.order)
      Some(pairs.head ::: pairs.last ::: sortedOthers.last :: Nil)
    }
    // 3
    else {
      val sortPairRanks = pairsRanks.sorted
      val highestPair = handState(sortPairRanks.last)
      val secondPair = handState(sortPairRanks.tail.head)
      val kickerCandidates = others ::: handState(sortPairRanks.head)
      val kicker = kickerCandidates.maxBy(_.rank.order)
      Some(kicker :: secondPair ::: highestPair)
    }
  }

  def apply(testCase: TestCase): Option[Combination] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val twoPairs = evaluate(allCards)
    Option.when(twoPairs.isDefined)(new TwoPairs(testCase) {
      override val combination: List[Card] = twoPairs.get
    })
  }
}

sealed abstract case class ThreeOfKind private(testCase: TestCase) extends Combination {
  override val name: String = ThreeOfKind.name
}

object ThreeOfKind {
  val name = "Three of Kind"

  def evaluate(list: List[Card]): Option[List[Card]] = {
    val handState = HandState.evaluateState(list)
    handState.find {
      case (_, listCards) => listCards.length == 3
    }.map {
      case (rank, triple) =>
        val others = handState.filter {
          case (entryRank, _) => entryRank != rank
        }
        val sortedOthers = others.values.toList.flatten.sortBy(_.rank.order)
        sortedOthers.take(2) ::: triple
    }
  }

  def apply(testCase: TestCase): Option[Combination] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val threeOfKind = evaluate(allCards)
    Option.when(threeOfKind.isDefined)(new ThreeOfKind(testCase) {
      override val combination: List[Card] = threeOfKind.get
    })
  }
}

sealed abstract case class Flush private(testCase: TestCase) extends Combination {
  override val name: String = Flush.name
}

object Flush {
  val name = "Flush"

  def evaluate(list: List[Card]): Option[List[Card]] = {
    val diamonds = list.filter(_.suit == Diamonds)
    val clubs = list.filter(_.suit == Clubs)
    val heards = list.filter(_.suit == Hearts)
    val spades = list.filter(_.suit == Spades)

    if (diamonds.length >= 5) Some(diamonds)
    else if (clubs.length >= 5) Some(clubs)
    else if (heards.length >= 5) Some(heards)
    else if (spades.length >= 5) Some(spades)
    else None
  }

  def apply(testCase: TestCase): Option[Combination] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val flush = evaluate(allCards)
    Option.when(flush.isDefined)(new Flush(testCase) {
      override val combination: List[Card] = flush.get
    })
  }
}

sealed abstract case class FullHouse private(testCase: TestCase) extends Combination {
  override val name: String = FullHouse.name
}

object FullHouse {
  val name = "Full House"

  def evaluate(list: List[Card]): Option[List[Card]] = {
    val handState = HandState.evaluateState(list)
    val triples = handState.filter {
      case (_, listCards) => listCards.length == 3
    }
    if (triples.isEmpty) None
    else if (triples.size == 2) {
      val sortedKeys = triples.keys.toList.sorted
      val (lowerRank, higherRank) = (sortedKeys.head, sortedKeys.last)
      Some(triples(lowerRank).take(2) ::: triples(higherRank))
    }
    // size == 1
    else {
      val pairs = handState.filter {
        case (_, listCards) => listCards.length == 2
      }
      if (pairs.isEmpty) None
      else if (pairs.size == 2) {
        val sortedKeys = pairs.keys.toList.sorted
        val (_, higherRank) = (sortedKeys.head, sortedKeys.last)
        Some(handState(higherRank).take(2) ::: triples(triples.keys.head))
      }
      // size == 1
      else {
        Some(handState(pairs.keys.head) ::: triples(triples.keys.head))
      }
    }
  }

  def apply(testCase: TestCase): Option[Combination] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val fullHouse = evaluate(allCards)
    Option.when(fullHouse.isDefined)(new FullHouse(testCase) {
      override val combination: List[Card] = fullHouse.get
    })
  }
}

sealed abstract case class FourOfAKind private(testCase: TestCase) extends Combination {
  override val name: String = FourOfAKind.name
}

object FourOfAKind {
  val name = "Four of a kind"

  def evaluate(list: List[Card]): Option[List[Card]] = {
    def inner(start: Card, tail: List[Card]): List[Card] = {
      var it = start
      tail.takeWhile { elem =>
        val prev = it
        it = elem
        elem.rank.order == prev.rank.order
      }
    }

    val sorted = list.sortBy(_.rank.order)
    val fromFirstWhile = inner(sorted.head, sorted.tail)
    if (fromFirstWhile.length == 3) Some(sorted.last :: sorted.head :: fromFirstWhile)
    // 6
    else if (fromFirstWhile.isEmpty) {
      val fromSecondWhile = inner(sorted.tail.head, sorted.tail.tail)
      if (fromSecondWhile.length == 3) Some(sorted.last :: sorted.tail.head :: fromSecondWhile)
      else if (fromSecondWhile.isEmpty) {
        val fromThirdWhile = inner(sorted.tail.tail.head, sorted.tail.tail.tail)
        if (fromThirdWhile.length == 3) Some(sorted.last :: sorted.tail.tail.head :: fromThirdWhile)
        else if (fromThirdWhile.isEmpty) {
          val fromFourthWhile = inner(sorted.tail.tail.tail.head, sorted.tail.tail.tail.tail)
          if (fromFourthWhile.length == 3) Some(sorted.tail.tail.head :: sorted.tail.tail.tail.head :: fromFourthWhile)
          else None
        }
        else None
      }
      else if (fromSecondWhile.length == 1) {
        val fromFourthWhile = inner(sorted.tail.tail.tail.head, sorted.tail.tail.tail.tail)
        if (fromFourthWhile.length == 3) Some(sorted.tail.tail.head :: sorted.tail.tail.tail.head :: fromFourthWhile)
        else None
      }
      else None
    }
    // 5
    else if (fromFirstWhile.length == 1) {
      val fromThirdWhile = inner(sorted.tail.tail.head, sorted.tail.tail.tail)
      if (fromThirdWhile.length == 3) Some(sorted.last :: sorted.tail.tail.head :: fromThirdWhile)
      else if (fromThirdWhile.isEmpty) {
        val fromFourthWhile = inner(sorted.tail.tail.tail.head, sorted.tail.tail.tail.tail)
        if (fromFourthWhile.length == 3) Some(sorted.tail.tail.head :: sorted.tail.tail.tail.head :: fromFourthWhile)
        else None
      }
      else None
    }
    else {
      val fromFourthWhile = inner(sorted.tail.tail.tail.head, sorted.tail.tail.tail.tail)
      if (fromFourthWhile.length == 3) Some(sorted.tail.tail.head :: sorted.tail.tail.tail.head :: fromFourthWhile)
      else None
    }
  }

  def apply(testCase: TestCase): Option[Combination] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val fourOfKind = evaluate(allCards)
    Option.when(fourOfKind.isDefined)(new FourOfAKind(testCase) {
      override val combination: List[Card] = fourOfKind.get
    })
  }
}

sealed abstract case class Straight private(testCase: TestCase) extends Combination {
  override val name: String = Straight.name
}

object Straight {
  val name = "Straight"

  def evaluate(list: List[Card]): Option[List[Card]] = {
    def inner(start: Card, tail: List[Card]): List[Card] = {
      var it = start
      tail.takeWhile { elem =>
        val prev = it
        it = elem
        elem.rank.order + 1 == prev.rank.order
      }
    }

    val sorted = list.sortBy(_.rank.order)
    val whileFromStart = inner(sorted.head, sorted.tail)
    if (whileFromStart.length > 1 && whileFromStart.length <= 3) None
    // 0
    else if (whileFromStart.isEmpty) {
      val whileFromSecond = inner(sorted.tail.head, sorted.tail.tail)
      if (whileFromSecond.nonEmpty && whileFromSecond.length <= 3) None
      // 0
      else {
        val whileFromThird = inner(sorted.tail.tail.head, sorted.tail.tail.tail)
        if (whileFromThird.length == 4) Some(sorted.tail.tail.head :: whileFromThird)
        else None
      }
    }
    // 1
    else {
      val whileFromThird = inner(sorted.tail.tail.head, sorted.tail.tail.tail)
      if (whileFromThird.length == 4) Some(sorted.tail.tail.head :: whileFromThird)
      else None
    }
  }

  def apply(testCase: TestCase): Option[Combination] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val straight = evaluate(allCards)
    Option.when(straight.isDefined)(new Straight(testCase) {
      override val combination: List[Card] = straight.get
    })
  }
}

sealed abstract case class StraightFlush private(testCase: TestCase) extends Combination {
  override val name: String = StraightFlush.name
}

object StraightFlush {
  val name = "Straight"

  def evaluate(list: List[Card]): Option[List[Card]] = {
    Flush.evaluate(list).flatMap(flush => {
      Straight.evaluate(flush)
    })
  }

  def apply(testCase: TestCase): Option[Combination] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val straightFlush = evaluate(allCards)
    Option.when(straightFlush.isDefined)(new StraightFlush(testCase) {
      override val combination: List[Card] = straightFlush.get
    })
  }
}

sealed abstract case class RoyalFlush private(testCase: TestCase) extends Combination {
  override val name: String = RoyalFlush.name
}

object RoyalFlush {
  val name = "Royal Flush"

  def evaluate(list: List[Card]): Option[List[Card]] = {
    Flush.evaluate(list).flatMap(flush => {
      val ten = flush.find(_.rank == Ten)
      val jack = flush.find(_.rank == Jack)
      val queen = flush.find(_.rank == Queen)
      val king = flush.find(_.rank == King)
      val ace = flush.find(_.rank == Ace)

      if (ten.isDefined && jack.isDefined && queen.isDefined && king.isDefined && ace.isDefined) {
        Some(List(ten.get, jack.get, queen.get, king.get, ace.get))
      } else {
        None
      }
    })
  }

  def apply(testCase: TestCase): Option[Combination] = {
    val allCards = testCase.board.value ::: testCase.hand.value
    val royalFlush = evaluate(allCards)
    Option.when(royalFlush.isDefined)(new RoyalFlush(testCase) {
      override val combination: List[Card] = royalFlush.get
    })
  }
}

case object HandState {
  def evaluateState(list: List[Card]): Map[String, List[Card]] = list.groupBy(_.rank.value)
}
