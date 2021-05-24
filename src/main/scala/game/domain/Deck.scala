package edu.agus.poker
package game.domain

import cats.effect.Sync
import cats.syntax.all._

import scala.util.Random

final case class Deck(cards: Array[Card])

object Deck {
  def apply(): Deck = {
    val cards = for {
      suit <- Array(Diamonds, Clubs, Hearts, Spades)
      rank <- Array(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)
    } yield Card(rank, suit)
    Deck(cards)
  }

  def retrieveCard[F[_] : Sync](deck: Deck): F[(Deck, Card)] = {
    val cards = deck.cards
    for {
      index <- Sync[F].pure(Random.between(0, cards.length))
      card = cards(index)
      others = cards.filter(_ != card)
    } yield (Deck(others), card)
  }

  // TODO: Generalize it
  def retrieveTexasHand[F[_] : Sync](deck: Deck): F[(Deck, TexasHand)] = for {
    deckCard <- retrieveCard(deck)
    (others1, card1) = deckCard
    deckCard2 <- retrieveCard(others1)
    (others2, card2) = deckCard2
  } yield (others2, TexasHand(card1, card2))


  def retrieveTexasBoard[F[_] : Sync](deck: Deck): F[(Deck, Board)] = for {
    deckCard <- retrieveCard(deck)
    (others1, card1) = deckCard
    deckCard2 <- retrieveCard(others1)
    (others2, card2) = deckCard2
    deckCard3 <- retrieveCard(others2)
    (others3, card3) = deckCard3
  } yield (others3, Board(List(card1, card2, card3)))

}
