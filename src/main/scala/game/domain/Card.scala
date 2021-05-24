package edu.agus.poker
package game.domain

final case class Card private(rank: Rank, suit: Suit)

object Card {
  def apply(rank: Rank, suit: Suit): Card = new Card(rank, suit)
}