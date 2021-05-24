package edu.agus.poker
package user

import java.time.LocalDate
import java.util.UUID

final case class UserDTO(username: String, password: String)

final case class User(id: UUID, username: String, password: String, budget: BigDecimal, createdAt: LocalDate, isAdmin: Boolean)

final case class UserSession(user: User, inGame: Boolean = false)

final case class Budget(budget: BigDecimal)

final case class UsernameID(id: UUID, username: String)


