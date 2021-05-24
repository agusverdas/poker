package edu.agus.poker

import game.domain.{Card, Combination, Flush, FourOfAKind, FullHouse, HandState, HighCard, Pair, Rank, RoyalFlush, Straight, StraightFlush, ThreeOfKind, TwoPairs}

import scala.annotation.tailrec

package object game {
  implicit def combinationComparator[A <: Combination]: Ordering[A] = (x: A, y: A) => {
    if (x.name == RoyalFlush.name && y.name != RoyalFlush.name) 1
    else if (x.name == RoyalFlush.name && y.name == RoyalFlush.name) 0
    else if (y.name == RoyalFlush.name && x.name != RoyalFlush.name) -1
    else if (x.name == StraightFlush.name && y.name != StraightFlush.name) 1
    else if (x.name == StraightFlush.name && y.name == StraightFlush.name) compareHighCard(x.combination, y.combination)
    else if (y.name == StraightFlush.name && x.name != StraightFlush.name) -1
    else if (x.name == Straight.name && y.name != Straight.name) 1
    else if (x.name == Straight.name && y.name == Straight.name) compareHighCard(x.combination, y.combination)
    else if (y.name == Straight.name && x.name != Straight.name) -1
    else if (x.name == FourOfAKind.name && y.name != FourOfAKind.name) 1
    else if (x.name == FourOfAKind.name && y.name == FourOfAKind.name) compareNumberOfAKind(x.combination, y.combination, 4)
    else if (y.name == FourOfAKind.name && x.name != FourOfAKind.name) -1
    else if (x.name == FullHouse.name && y.name != FullHouse.name) 1
    else if (x.name == FullHouse.name && y.name == FullHouse.name) compareHighCard(x.combination, y.combination)
    else if (y.name == FullHouse.name && x.name != FullHouse.name) -1
    else if (x.name == Flush.name && y.name != Flush.name) 1
    else if (x.name == Flush.name && y.name == Flush.name) compareHighCard(x.combination, y.combination)
    else if (y.name == Flush.name && x.name != Flush.name) -1
    else if (x.name == ThreeOfKind.name && y.name != ThreeOfKind.name) 1
    else if (x.name == ThreeOfKind.name && y.name == ThreeOfKind.name) compareNumberOfAKind(x.combination, y.combination, 3)
    else if (y.name == ThreeOfKind.name && x.name != ThreeOfKind.name) -1
    else if (x.name == TwoPairs.name && y.name != TwoPairs.name) 1
    else if (x.name == TwoPairs.name && y.name == TwoPairs.name) compareTwoPairs(x, y)
    else if (y.name == TwoPairs.name && x.name != TwoPairs.name) -1
    else if (x.name == Pair.name && y.name != Pair.name) 1
    else if (x.name == Pair.name && y.name == Pair.name) compareNumberOfAKind(x.combination, y.combination, 2)
    else if (y.name == Pair.name && x.name != Pair.name) -1
    else if (x.name == HighCard.name && y.name != HighCard.name) 1
    else if (y.name == HighCard.name && x.name != HighCard.name) -1
    else compareHighCard(x.combination, y.combination)
  }

  private def compareTwoPairs[A <: Combination](x: A, y: A): Int = {
    val thisState = HandState.evaluateState(x.combination).values
    val thisPairs = thisState.filter(list => list.size == 2)
    val thisOther = thisState.filter(list => list.size != 2).flatten.toList
    val targetState = HandState.evaluateState(y.combination).values
    val targetPairs = targetState.filter(list => list.size == 2)
    val targetOther = targetState.filter(list => list.size != 2).flatten.toList
    val compareThisPairs = compareNumberOfAKind(thisPairs.head, thisPairs.tail.head, 2)
    val thisStrongestPair = {
      if (compareThisPairs < 0) thisPairs.tail.head
      else if (compareThisPairs > 0) thisPairs.head
      else thisPairs.head
    }
    val compareTargetPairs = compareNumberOfAKind(targetPairs.head, targetPairs.tail.head, 2)
    val targetStrongestPair = {
      if (compareTargetPairs < 0) targetPairs.tail.head
      else if (compareThisPairs > 0) targetPairs.head
      else targetPairs.head
    }
    val comparePairs = compareNumberOfAKind(thisStrongestPair, targetStrongestPair, 2)
    if (comparePairs != 0) comparePairs
    else compareHighCard(thisOther, targetOther)
  }

  private def compareHighCard(x: List[Card], y: List[Card]): Int = {
    val reversedX = x.sortBy(_.rank.order).reverse
    val reversedY = y.sortBy(_.rank.order).reverse
    val ranksX = reversedX.map(_.rank)
    val ranksY = reversedY.map(_.rank)
    @tailrec
    def inner(ranksX: List[Rank], ranksY: List[Rank]): Int ={
      if (ranksX.isEmpty && ranksY.isEmpty) 0
      else if (ranksX.head.order > ranksY.head.order) 1
      else if (ranksX.head.order < ranksY.head.order) -1
      else inner(ranksX.tail, ranksY.tail)
    }
    inner(ranksX, ranksY)
  }

  private def compareNumberOfAKind(x: List[Card], y: List[Card], number: Int): Int = {
    val thisState = HandState.evaluateState(x).values
    val thisTriples = thisState.filter(list => list.size == number).head
    val thisOther = thisState.filter(list => list.size != number).flatten.toList
    val targetState = HandState.evaluateState(y).values
    val targetTriples = targetState.filter(list => list.size == number).head
    val targetOther = targetState.filter(list => list.size != number).flatten.toList
    val compareTriples = compareHighCard(thisTriples, targetTriples)
    if (compareTriples != 0) compareTriples
    else compareHighCard(thisOther, targetOther)
  }
}
