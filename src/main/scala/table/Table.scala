package edu.agus.poker
package table


import user.{User, UsernameID}

import java.util.UUID

case class Table(id: UUID, game: GamePreparation, isActive: Boolean)

object Table {
  def initial: Table = Table(UUID.randomUUID(), GamePreparation.initial, isActive = false)
}

case class PlayerGamePreparation(player: UsernameID, confirmed: Boolean = false)

object PlayerGamePreparation {
  def initial(user: User): PlayerGamePreparation = PlayerGamePreparation(UsernameID(user.id, user.username))
}

case class GamePreparation(playerGames: List[PlayerGamePreparation])

object GamePreparation {
  def initial: GamePreparation = GamePreparation(Nil)
}
