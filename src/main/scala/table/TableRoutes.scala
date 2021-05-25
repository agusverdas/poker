package edu.agus.poker
package table

import state.Warehouse
import user.UserSession
import util.PokerRoutes
import util.PokerRoutes.isLoggedIn

import cats.effect.Sync
import cats.syntax.all._
import io.circe.generic.auto._
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl

final class TableRoutes[F[_] : Sync](tableService: TableService[F], sessions: Warehouse[UserSession, F]) extends Http4sDsl[F] with PokerRoutes[F] {
  override def routes: HttpRoutes[F] = HttpRoutes.of[F] {
    case req@GET -> Root / "tables" => isLoggedIn(req, sessions).flatMap {
      case None => BadRequest(NotLoggedInMessage)
      case Some(_) => tableService.tables.flatMap(tables => Ok(tables))
    }.handleErrorWith(x => BadRequest(s"Table Error $x"))

    case req@POST -> Root / "table" / UUIDVar(uuid) => isLoggedIn(req, sessions).flatMap {
      case None => BadRequest(NotLoggedInMessage)
      case Some(session) =>
        tableService.joinTable(session.user.id, uuid) *> Ok("You joined the table")
          .handleErrorWith(_ => BadRequest("Table Error"))
    }

    case req@PUT -> Root / "table" / UUIDVar(uuid) => isLoggedIn(req, sessions).flatMap {
      case None => BadRequest(NotLoggedInMessage)
      case Some(session) =>
        tableService.confirmGame(session.user.id, uuid) *> Ok("You confirmed the table")
          .handleErrorWith(_ => BadRequest("Table Error"))
    }

    case req@POST -> Root / "table" / "start" / UUIDVar(uuid) => isLoggedIn(req, sessions).flatMap {
      case None => BadRequest(NotLoggedInMessage)
      case Some(_) =>
        tableService.activateTable(uuid) *> Ok("You created the table")
          .handleErrorWith(_ => BadRequest("Table Error"))
    }
  }
}
