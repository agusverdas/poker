package edu.agus.poker
package game.domain

object Hand {
  val BoardSize = 5
  val MaxCardsOfSameRank = 4
  val TexasHandSize = 2

  val isHandUnique: List[Card] => Boolean = l => l.length == Set(l).size
  val validateCardsOfSameRank: List[Card] => Boolean = _.groupBy(_.suit).values.forall(_.size <= Hand.MaxCardsOfSameRank)
}
sealed abstract class Hand protected(val value: List[Card]) {
  def canEqual(other: Any): Boolean = other.isInstanceOf[Hand]

  override def equals(other: Any): Boolean = other match {
    case that: Hand =>
      (that canEqual this) &&
        value == that.value
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(value)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

sealed case class TexasHand(first: Card, second: Card) extends Hand(List(first, second))
