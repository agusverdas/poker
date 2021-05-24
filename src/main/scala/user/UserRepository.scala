package edu.agus.poker
package user

import cats.data.OptionT
import cats.implicits.catsSyntaxApply
import cats.syntax.all._
import doobie.implicits._
import util.implicits._
import doobie.implicits.javatime._
import doobie.{ConnectionIO, Fragment}

import java.util.UUID

trait UserRepository[F[_]] {
  def users: F[List[User]]

  def user(id: UUID): F[Option[User]]

  def user(username: String): F[Option[User]]

  def create(user: User): F[Option[User]]

  def updateBalance(id: UUID, amount: BigDecimal): F[Option[User]]
}

final class UserRepositoryH2 extends UserRepository[ConnectionIO] {
  val findAll: Fragment = fr"SELECT id, name, password, budget, createdAt, isAdmin FROM users"

  def findById(id: UUID): Fragment = findAll ++ fr" WHERE id = $id"

  def findByName(name: String): Fragment = findAll ++ fr" WHERE name = $name"

  def insert(user: User): Fragment = fr"INSERT INTO users(id, name, password) VALUES(${user.id}, ${user.username}, ${user.password})"

  def updateBudget(id: UUID, budget: BigDecimal) = fr"UPDATE users SET budget = $budget WHERE id = $id"

  override def users: ConnectionIO[List[User]] =
    findAll.query[User].to[List]

  override def user(id: UUID): ConnectionIO[Option[User]] =
    findById(id).query[User].option

  override def user(username: String): ConnectionIO[Option[User]] =
    findByName(username).query[User].option

  override def create(user: User): ConnectionIO[Option[User]] = {
    insert(user).update.run *> this.user(user.id)
  }

  // amount can be negative
  override def updateBalance(id: UUID, amount: BigDecimal): ConnectionIO[Option[User]] = {
    val optionT = for {
      user <- OptionT(this.user(id))
      _ <- OptionT.liftF(updateBudget(id, user.budget + amount).update.run)
      userFromDb <- OptionT(this.user(id))
    } yield userFromDb
    optionT.value
  }
}
