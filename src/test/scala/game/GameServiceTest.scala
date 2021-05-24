package edu.agus.poker
package game

import game.domain._

import org.scalatest.FunSuite

class GameServiceTest extends FunSuite {
  test("1") {
    val board = Board(List(
      Card(Two, Diamonds),
      Card(Three, Clubs),
      Card(Jack, Diamonds),
      Card(Ten, Spades),
      Card(Five, Hearts)
    ))
    val hand1 = TexasHand(Card(Ten, Diamonds), Card(Ten, Hearts))
    val x1 = TexasCase(board, hand1)
    val res1 = GameService.combination(x1)

    val hand2 = TexasHand(Card(Three, Diamonds), Card(Three, Hearts))
    val x2 = TexasCase(board, hand2)
    val res2 = GameService.combination(x2)
  }

}
