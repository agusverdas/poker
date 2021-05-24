package edu.agus.poker
package user

import state.Warehouse
import util.PokerRoutes

import cats.data.OptionT
import cats.effect.Sync
import cats.syntax.all._
import io.circe.generic.auto._
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl

import java.util.UUID


final class UserRoutes[F[_] : Sync](userService: UserService[F], sessions: Warehouse[UserSession, F]) extends Http4sDsl[F] with PokerRoutes[F] {
  def isLoggedIn(req: Request[F]): F[Option[UserSession]] = {
    val optionT = for {
      userId <- OptionT.fromOption[F](req.cookies.find(cookie => cookie.name == UserCookie))
      userSession <- OptionT(sessions.get(UUID.fromString(userId.content)))
    } yield userSession
    optionT.value
  }

  override def routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case req@GET -> Root / "users" =>
      isLoggedIn(req).flatMap {
        case None => BadRequest(NotLoggedInMessage)
        case Some(session) =>
          if (session.user.isAdmin) userService.findAllUsers.flatMap(users => Ok(users))
          else BadRequest("You are not a superuser")
      }

    case req@GET -> Root / "user" =>
      isLoggedIn(req).flatMap {
        case None => BadRequest(NotLoggedInMessage)
        case Some(session) => userService.findUser(session.user.id).flatMap(user => Ok(user)).handleErrorWith {
            _ => BadRequest("User error")
          }
      }

    case req@POST -> Root / "user" =>
      isLoggedIn(req).flatMap {
        case Some(_) => BadRequest("Log off to register new account")
        case None => (for {
          userDTO <- req.as[UserDTO]
          created <- userService.registerUser(userDTO)
          resp <- Created(created).map(_.addCookie(UserCookie, created.id.toString))
        } yield resp).handleErrorWith(
          x => BadRequest(s"User Error $x"))
      }

    case req@POST -> Root / "login" =>
      isLoggedIn(req).flatMap {
        case Some(_) => BadRequest("You already logged in")
        case None => (for {
          userDTO <- req.as[UserDTO]
          session <- userService.login(userDTO)
          resp <- Ok("Logged in").map(_.addCookie(UserCookie, session.user.id.toString))
        } yield resp).handleErrorWith(
          x => BadRequest(s"User Error $x"))
      }

    case req@GET -> Root / "logoff" =>
      isLoggedIn(req).flatMap {
        case None => BadRequest(NotLoggedInMessage)
        case Some(session) => userService.logoff(session.user.id).flatMap(user => Ok(user)).handleErrorWith {
          _ => BadRequest("User error")
        }
      }

    case req@PUT -> Root / "budget" =>
      isLoggedIn(req).flatMap {
        case None => BadRequest(NotLoggedInMessage)
        case Some(session) => (for {
          budget <- req.as[Budget]
          updated <- userService.updateBudget(session.user.id, budget)
          resp <- Ok(updated)
        } yield resp).handleErrorWith(
          _ => BadRequest("User Error"))
      }
  }
}
