package edu.agus.poker
package game

import game.domain.{BetDTO, BetIsNegative}
import state.Warehouse
import user.{UserNotFound, UserSession}
import util.PokerRoutes
import util.PokerRoutes.isLoggedIn

import cats.effect.Sync
import cats.syntax.all._
import io.circe.generic.auto._
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl

final class GameRoutes[F[_] : Sync](gameService: GameService[F], sessions: Warehouse[UserSession, F]) extends Http4sDsl[F] with PokerRoutes[F] {
  override def routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case req@GET -> Root / "games" => isLoggedIn(req, sessions).flatMap {
      case None => BadRequest(NotLoggedInMessage)
      case Some(_) => gameService.games.flatMap(games => Ok(games))
    }

    case req@POST -> Root / "small" / UUIDVar(uuid) => isLoggedIn(req, sessions).flatMap {
      case None => BadRequest(NotLoggedInMessage)
      case Some(session) => (for {
        bet <- req.as[BetDTO]
        _ <- if (bet.bet <= 0) Sync[F].raiseError(BetIsNegative) else ().pure[F]
        _ <- gameService.blind(SmallBlind, uuid, session.user.id, bet.bet)
      } yield ()) *> Ok("Small blind was done")
    }.handleErrorWith {
      case BetIsNegative => BadRequest(s"Bet is negative or equal to zero")
      case GameNotFound => NotFound(s"Game with id : $uuid was not found")
      case UserNotFound => NotFound("Can't find user")
      case UserNotInGame => BadRequest(s"User not in game with id : $uuid")
    }

    case req@POST -> Root / "large" / UUIDVar(uuid) => isLoggedIn(req, sessions).flatMap {
      case None => BadRequest(NotLoggedInMessage)
      case Some(session) => (for {
        bet <- req.as[BetDTO]
        _ <- if (bet.bet <= 0) Sync[F].raiseError(BetIsNegative) else ().pure[F]
        _ <- gameService.blind(LargeBlind, uuid, session.user.id, bet.bet)
      } yield ()) *> Ok("Large blind was done")
    }.handleErrorWith {
      case BetIsNegative => BadRequest(s"Bet is negative or equal to zero")
      case GameNotFound => NotFound(s"Game with id : $uuid was not found")
      case UserNotFound => NotFound("Can't find user")
      case UserNotInGame => BadRequest(s"User not in game with id : $uuid")
    }

    case req@POST -> Root / "call" / UUIDVar(uuid) => isLoggedIn(req, sessions).flatMap {
      case None => BadRequest(NotLoggedInMessage)
      case Some(session) => (for {
        bet <- req.as[BetDTO]
        _ <- gameService.call(uuid, session.user.id, bet.bet)
      } yield ()) *> Ok("Call was done")
    }.handleErrorWith {
      case BetIsNegative => BadRequest(s"Bet is negative or equal to zero")
      case GameNotFound => NotFound(s"Game with id : $uuid was not found")
      case UserNotFound => NotFound("Can't find user")
      case UserNotInGame => BadRequest(s"User not in game with id : $uuid")
    }

    case req@GET -> Root / "turn" / UUIDVar(uuid) => isLoggedIn(req, sessions).flatMap {
      case None => BadRequest(NotLoggedInMessage)
      case Some(_) => gameService.turn(uuid) *> Ok("Turn was done")
    }.handleErrorWith {
      case GameNotFound => NotFound(s"Game with id : $uuid was not found")
      case UserNotFound => NotFound("Can't find user")
      case UserNotInGame => BadRequest(s"User not in game with id : $uuid")
      case WrongGameState(state) => BadRequest(s"Game is in wrong state : $state")
    }

    case req@GET -> Root / "reaver" / UUIDVar(uuid) => isLoggedIn(req, sessions).flatMap {
      case None => BadRequest(NotLoggedInMessage)
      case Some(_) => gameService.reaver(uuid) *> Ok("Reaver was done")
    }.handleErrorWith {
      case GameNotFound => NotFound(s"Game with id : $uuid was not found")
      case UserNotFound => NotFound("Can't find user")
      case UserNotInGame => BadRequest(s"User not in game with id : $uuid")
      case WrongGameState(state) => BadRequest(s"Game is in wrong state : $state")
    }

    case req@GET -> Root / "opening" / UUIDVar(uuid) => isLoggedIn(req, sessions).flatMap {
      case None => BadRequest(NotLoggedInMessage)
      case Some(_) => gameService.opening(uuid).flatMap {
        case (user, combination) => Ok(s"User $user won with combination ${combination.name}, ${combination.combination}")
      }
    }.handleErrorWith {
      case GameNotFound => NotFound(s"Game with id : $uuid was not found")
      case UserNotFound => NotFound("Can't find user")
      case UserNotInGame => BadRequest(s"User not in game with id : $uuid")
    }
  }
}
