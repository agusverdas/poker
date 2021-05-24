package edu.agus.poker
package user

import state.Warehouse

import cats.effect.{Resource, Sync}
import cats.syntax.all._
import doobie.implicits._
import doobie.{ConnectionIO, Transactor}

import java.time.LocalDate
import java.util.UUID

sealed trait UserError extends Exception

case object UnableToLogInUser extends UserError

case object UserSessionNotFound extends UserError

case object UserNotFound extends UserError

case object IncorrectUserPassword extends UserError

case object UserAlreadyLoggedIn extends UserError

case object UserAlreadyLoggedOff extends UserError

trait UserService[F[_]] {
  def userSession(userId: UUID): F[UserSession]

  def registerUser(user: UserDTO): F[User]

  def login(user: UserDTO): F[UserSession]

  def logoff(userId: UUID): F[Unit]

  def findAllUsers: F[List[User]]

  def findUser(id: UUID): F[User]

  def updateBudget(id: UUID, amount: Budget): F[User]
}

final class UserServiceImpl[F[_] : Sync](sessions: Warehouse[UserSession, F],
                                         transactor: Resource[F, Transactor[F]],
                                         userRepository: UserRepository[ConnectionIO]) extends UserService[F] {
  override def userSession(userId: UUID): F[UserSession] = {
    val sessionOpt = sessions.get(userId)
    for {
      session <- sessionOpt
      result <- session match {
        case None => Sync[F].raiseError(UserSessionNotFound)
        case Some(session) => session.pure[F]
      }
    } yield result
  }

  override def registerUser(userDTO: UserDTO): F[User] = {
    val user = User(UUID.randomUUID(), userDTO.username, userDTO.password, 0, LocalDate.now(), isAdmin = false)
    for {
      userOpt <- transactor.use(xa => userRepository.create(user).transact(xa))
      user <- userOpt match {
        case None => Sync[F].raiseError(UserNotFound)
        case Some(x) => x.pure[F]
      }
      _ <- sessions.put(user.id, UserSession(user))
    } yield user
  }

  override def login(userDTO: UserDTO): F[UserSession] = {
    for {
      sessionsMap <- sessions.entries
      sessionEntryOpt <- sessionsMap.find {
        case (_, userSession) => userSession.user.username == userDTO.username
      }.pure[F]
      _ <- sessionEntryOpt match {
        case Some(_) => Sync[F].raiseError(UserAlreadyLoggedIn)
        case None => ().pure[F]
      }
      user <- transactor.use(xa => userRepository.user(userDTO.username).transact(xa))
      resultOpt <- user match {
        case None => Sync[F].raiseError(UserNotFound)
        case Some(x) if x.password != userDTO.password => Sync[F].raiseError(IncorrectUserPassword)
        case Some(x) =>
          sessions.put(x.id, UserSession(x)) *> sessions.get(x.id)
      }
      result <- resultOpt match {
        case None => Sync[F].raiseError(UnableToLogInUser)
        case Some(x) => x.pure[F]
      }
    } yield result
  }

  override def logoff(userId: UUID): F[Unit] = {
    val sessionOpt = sessions.get(userId)
    for {
      session <- sessionOpt
      _ <- session match {
        case None => Sync[F].raiseError(UserAlreadyLoggedOff)
        case Some(_) =>
          sessions.remove(userId)
      }
    } yield ()
  }

  override def findAllUsers: F[List[User]] = for {
    users <- transactor.use(xa => userRepository.users.transact(xa))
  } yield users

  override def findUser(id: UUID): F[User] = for {
    userOpt <- transactor.use(xa => userRepository.user(id).transact(xa))
    user <- userOpt match {
      case None => Sync[F].raiseError(UserNotFound)
      case Some(session) => session.pure[F]
    }
  } yield user

  override def updateBudget(id: UUID, amount: Budget): F[User] = for {
    userOpt <- transactor.use(xa => userRepository.updateBalance(id, amount.budget).transact(xa))
    user <- userOpt match {
      case None => Sync[F].raiseError(UserNotFound)
      case Some(session) => session.pure[F]
    }
    _ <- sessions.put(user.id, UserSession(user))
  } yield user
}
