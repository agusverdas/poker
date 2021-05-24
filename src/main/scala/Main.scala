package edu.agus.poker

import db.DbTransactor
import game.domain.Game
import game.{GameRoutes, GameService}
import state.Warehouse
import table.{TableRoutes, TableService}
import user.{UserRepositoryH2, UserRoutes, UserServiceImpl, UserSession}
import util.PokerModelDDL.CreateUserTable

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.toSemigroupKOps
import doobie.Fragment
import doobie.implicits._
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext

object Main extends IOApp {

  val ddl = Fragment.const(CreateUserTable)

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sessions <- Warehouse.of[UserSession, IO]
      games <- Warehouse.of[Game, IO]
      h2Repo = new UserRepositoryH2()
      transactor = DbTransactor.make[IO]
      userService = new UserServiceImpl[IO](sessions, transactor, h2Repo)
      gameService <- GameService.of[IO](games, sessions, userService)
      tablesService <- TableService.of[IO](sessions, games, gameService)
      userRoutes = new UserRoutes[IO](userService, sessions)
      tableRoutes = new TableRoutes[IO](tablesService, sessions)
      gameRoutes = new GameRoutes[IO](gameService, sessions)
      appRoutes = { userRoutes.routes <+> tableRoutes.routes <+> gameRoutes.routes }.orNotFound
      _ <- transactor.use { xa =>
        for {
          _ <- ddl.update.run.transact(xa)
        } yield ()
      }
      _ <- BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9001, host = "localhost")
        .withHttpApp(appRoutes)
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    } yield ExitCode.Success
  }
}
