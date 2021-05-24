package edu.agus.poker
package game.domain

sealed case class Board(value: List[Card])

object Board extends {
  def initial: Board = Board(Nil)
}