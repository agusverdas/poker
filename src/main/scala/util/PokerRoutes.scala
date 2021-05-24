package edu.agus.poker
package util

import state.Warehouse
import user.UserSession

import cats.Monad
import cats.data.OptionT
import org.http4s.{HttpRoutes, Request}

import java.util.UUID

trait PokerRoutes[F[_]] {
  protected val UserCookie: String = PokerRoutes.UserCookie
  protected val NotLoggedInMessage = "You are not logged in"
  def routes: HttpRoutes[F]
}

object PokerRoutes {
  private val UserCookie = "PokerCookie"
  def isLoggedIn[F[_] : Monad](req: Request[F], sessions: Warehouse[UserSession, F]): F[Option[UserSession]] = {
    val optionT = for {
      userId <- OptionT.fromOption[F](req.cookies.find(cookie => cookie.name == UserCookie))
      userSession <- OptionT(sessions.get(UUID.fromString(userId.content)))
    } yield userSession
    optionT.value
  }
}
