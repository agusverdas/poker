package edu.agus.poker
package table

import game.GameService
import state.Warehouse
import user.{User, UserSession, UserSessionNotFound}

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Timer}
import cats.syntax.all._
import edu.agus.poker.game.domain.Game

import java.util.UUID
import scala.concurrent.duration.DurationInt

case object TableNotFound extends Exception

case object TableIsPlaying extends Exception

case object PlayerNotOnTable extends Exception

case object AllPlayersShouldConfirmGame extends Exception

trait TableService[F[_]] {
  def tables: F[List[Table]]

  def joinTable(userId: UUID, tableId: UUID): F[Unit]

  def confirmGame(userId: UUID, tableId: UUID): F[Unit]

  def activateTable(tableId: UUID): F[Unit]
}

object TableService {
  private val CheckUsersEvery = 1.second
  private val MaxPlayersOnTable = 8

  def of[F[_] : Concurrent : Timer](sessionWarehouse: Warehouse[UserSession, F], gameWarehouse: Warehouse[Game, F], gameService: GameService[F]): F[TableService[F]] = for {
    list <- Ref.of(List.empty[Table])
    service = new TableService[F] {
      override def tables: F[List[Table]] = list.get

      override def joinTable(userId: UUID, tableId: UUID): F[Unit] = for {
        sessionOpt <- sessionWarehouse.get(userId)
        session <- sessionOpt match {
          case None => Concurrent[F].raiseError(UserSessionNotFound)
          case Some(x) => x.pure[F]
        }
        tableOpt <- tables.flatMap(ts => ts.find(_.id == tableId).pure[F])
        table <- tableOpt match {
          case None => Concurrent[F].raiseError(TableNotFound)
          case Some(x) if x.isActive => Concurrent[F].raiseError(TableIsPlaying)
          case Some(x) => x.pure[F]
        }
        _ <- list.update(refList =>
          refList.map(t => if (t.id == table.id && t.game.playerGames.length < MaxPlayersOnTable) {
            addPlayerToTable(t, session.user)
          } else t)
        )
        _ <- Concurrent[F].start(Timer[F].sleep(1.minute) *> list.update(refList =>
          refList.map(t => if (t.id == table.id && playerNotConfirmed(t, session.user)) {
            kickPlayerFromTable(t, session.user)
          } else t)))
      } yield ()

      override def confirmGame(userId: UUID, tableId: UUID): F[Unit] = for {
        sessionOpt <- sessionWarehouse.get(userId)
        session <- sessionOpt match {
          case None => Concurrent[F].raiseError(UserSessionNotFound)
          case Some(x) => x.pure[F]
        }
        tableOpt <- tables.flatMap(ts => ts.find(_.id == tableId).pure[F])
        table <- tableOpt match {
          case None => Concurrent[F].raiseError(TableNotFound)
          case Some(x) if !x.game.playerGames.exists(game => game.player.id == userId) => Concurrent[F].raiseError(PlayerNotOnTable)
          case Some(x) => x.pure[F]
        }
        _ <- list.update(refList =>
          refList.map(t => if (t.id == table.id) {
            confirm(t, session.user)
          } else t)
        )
      } yield ()

      override def activateTable(tableId: UUID): F[Unit] = for {
        tableOpt <- tables.flatMap(ts => ts.find(_.id == tableId).pure[F])
        table <- tableOpt match {
          case None => Concurrent[F].raiseError(TableNotFound)
          case Some(x) => x.pure[F]
        }
        playersPrep = table.game.playerGames
        tableGame <- if (playersPrep.exists(!_.confirmed)) {
          Concurrent[F].raiseError(AllPlayersShouldConfirmGame)
        } else {
          gameService.createGame(table)
        }
        (updTable, game) = tableGame
        _ <- list.update(refList =>
          refList.map(t => if (t.id == updTable.id) {
            updTable
          } else t)
        )
        _ <- gameWarehouse.put(game.id, game)
      } yield ()
    }
    _ <- Concurrent[F].start((Timer[F].sleep(CheckUsersEvery) *> autocreateTables(list, sessionWarehouse)).foreverM[Unit])
  } yield service

  // TODO: LENSES
  private def addPlayerToTable(table: Table, user: User): Table = {
    table.copy(game =
      table.game.copy(playerGames =
        PlayerGamePreparation.initial(user) :: table.game.playerGames
      )
    )
  }

  private def playerNotConfirmed(table: Table, user: User): Boolean =
    table.game.playerGames.exists(x => x.player.id == user.id && !x.confirmed)

  private def kickPlayerFromTable(table: Table, user: User): Table = {
    table.copy(game =
      table.game.copy(playerGames =
        table.game.playerGames.filter(_.player.id != user.id)
      )
    )
  }

  private def confirm(table: Table, user: User): Table = {
    table.copy(game =
      table.game.copy(playerGames =
        table.game.playerGames.map(x => {
          if (x.player.id == user.id) x.copy(confirmed = true)
          else x
        })
      )
    )
  }

  private def autocreateTables[F[_] : Monad](tables: Ref[F, List[Table]], sessionsWarehouse: Warehouse[UserSession, F]): F[Unit] = {
    for {
      playersNotInGame <- sessionsWarehouse.entries.map(sessions => sessions.count {
        case (_, session) => !session.inGame
      })
      notPlayingTables <- tables.get.map(_.count(table => !table.isActive))
      _ <- if (notPlayingTables == 0 && playersNotInGame > 0) {
        tables.update(list => list.appended(Table.initial))
      } else {
        ().pure[F]
      }
    } yield ()
  }
}
