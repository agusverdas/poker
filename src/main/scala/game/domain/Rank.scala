package edu.agus.poker
package game.domain

sealed trait Rank {
  def value: String
  def order: Int
}

object Rank {
  def apply(rank: String): Option[Rank] = {
    rank match {
      case "2" => Some(Two)
      case "3" => Some(Three)
      case "4" => Some(Four)
      case "5" => Some(Five)
      case "6" => Some(Six)
      case "7" => Some(Seven)
      case "8" => Some(Eight)
      case "9" => Some(Nine)
      case "10" => Some(Ten)
      case "J" => Some(Jack)
      case "Q" => Some(Queen)
      case "K" => Some(King)
      case "A" => Some(Ace)
      case _ => None
    }
  }
}

case object Two extends Rank {
  override val value: String = "2"

  override val order: Int = 0
}
case object Three extends Rank {
  override val value: String = "3"
  override val order: Int = 1
}
case object Four extends Rank {
  override val value: String = "4"
  override val order: Int = 2
}
case object Five extends Rank {
  override val value: String = "5"
  override val order: Int = 3
}
case object Six extends Rank {
  override val value: String = "6"
  override val order: Int = 4
}
case object Seven extends Rank {
  override val value: String = "7"
  override val order: Int = 5
}
case object Eight extends Rank {
  override val value: String = "8"
  override val order: Int = 6
}
case object Nine extends Rank {
  override val value: String = "9"
  override val order: Int = 7
}
case object Ten extends Rank {
  override val value: String = "T"
  override val order: Int = 8
}
case object Jack extends Rank {
  override val value: String = "J"
  override val order: Int = 9
}
case object Queen extends Rank {
  override val value: String = "Q"
  override val order: Int = 10
}
case object King extends Rank {
  override val value: String = "K"
  override val order: Int = 11
}
case object Ace extends Rank {
  override val value: String = "A"
  override val order: Int = 12
}
