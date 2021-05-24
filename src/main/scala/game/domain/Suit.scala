package edu.agus.poker
package game.domain

sealed trait Suit {
  def value: String
}
object Suit {
  def apply(suit: String): Option[Suit] = {
    suit match {
      case "d" => Some(Diamonds)
      case "c" => Some(Clubs)
      case "h" => Some(Hearts)
      case "s" => Some(Spades)
      case _ => None
    }
  }
}

case object Diamonds extends Suit {
  override val value : String = "d"
}
case object Clubs extends Suit {
  override val value: String = "c"
}
case object Hearts extends Suit {
  override val value: String = "h"
}
case object Spades extends Suit {
  override val value: String = "s"
}
