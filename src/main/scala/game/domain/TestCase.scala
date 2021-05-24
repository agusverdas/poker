package edu.agus.poker
package game.domain

sealed trait TestCase {
  def board: Board
  def hand: Hand
}
sealed case class TexasCase(board: Board, hand: TexasHand) extends TestCase
