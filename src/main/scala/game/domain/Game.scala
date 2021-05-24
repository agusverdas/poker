package edu.agus.poker
package game.domain

import user.UsernameID

import java.util.UUID

sealed trait GameState

case object BetIsNegative extends Exception

case object Preflop extends GameState

case object Flop extends GameState

case object Turn extends GameState

case object Reaver extends GameState

case class BetDTO(bet: BigDecimal)

case class Blind(player: UsernameID, bet: BigDecimal = 0, wasDone: Boolean = false)

case class PlayerGame(player: UsernameID, hand: TexasHand, bet: BigDecimal = 0, finish: Boolean = false)

case class Game(id: UUID,
                dealer: UsernameID,
                smallBlind: Blind,
                largeBlind: Blind,
                players: List[UsernameID],
                board: Board = Board.initial,
                playerGames: List[PlayerGame] = Nil,
                deck: Deck = Deck(),
                bank: BigDecimal = 0,
                state: GameState = Preflop)
