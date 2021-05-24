package edu.agus.poker
package game

import game.domain._
import state.Warehouse
import table.Table
import user._

import cats.effect.Sync
import cats.syntax.all._

import java.util.UUID
import scala.util.Random

case object GameNotFound extends Exception

case object NotEnoughPlayers extends Exception

case class WrongGameState(actual: GameState) extends Exception

case object UserNotInGame extends Exception

case object IncorrectPlayer extends Exception

case object SmallBlindWasNotDoneYet extends Exception

trait GameService[F[_]] {
  def games: F[List[Game]]

  def createGame(table: Table): F[(Table, Game)]

  def smallBlind(gameId: UUID, userId: UUID, bet: BigDecimal): F[Blind]

  def largeBlind(gameId: UUID, userId: UUID, bet: BigDecimal): F[Blind]

  def opening(gameId: UUID): F[(UsernameID, Combination)]

  def playerHand(gameId: UUID, userId: UUID): F[Hand]

  def raise(gameId: UUID, userId: UUID, bet: BigDecimal): F[Unit]

  def call(gameId: UUID, userId: UUID, bet: BigDecimal): F[Unit]

  def check(gameId: UUID, userId: UUID): F[Unit]

  def turn(gameId: UUID): F[Unit]

  def reaver(gameId: UUID): F[Unit]
}

object GameService {
  private val MinPlayers = 4

  private[game] def combination(texasCase: TexasCase): Combination =
    RoyalFlush(texasCase).getOrElse(
      StraightFlush(texasCase).getOrElse(
        Straight(texasCase).getOrElse(
          FourOfAKind(texasCase).getOrElse(
            FullHouse(texasCase).getOrElse(
              Flush(texasCase).getOrElse(
                ThreeOfKind(texasCase).getOrElse(
                  TwoPairs(texasCase).getOrElse(
                    Pair(texasCase).getOrElse(
                      HighCard(texasCase)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  def of[F[_] : Sync](gameWarehouse: Warehouse[Game, F], sessionWarehouse: Warehouse[UserSession, F], userService: UserService[F]): F[GameService[F]] = new GameService[F] {
    override def games: F[List[Game]] = for {
      entries <- gameWarehouse.entries
    } yield entries.values.toList

    override def createGame(table: Table): F[(Table, Game)] = {
      def initChooseDealer(players: List[UsernameID]): F[List[UsernameID]] = for {
        index <- Random.between(0, players.length).pure[F]
        dealer = players(index)
        others = players.filter(_ != dealer)
      } yield dealer :: others

      val players: List[UsernameID] = table.game.playerGames.map(_.player)
      for {
        _ <- if (players.length < MinPlayers) {
          Sync[F].raiseError(NotEnoughPlayers)
        } else ().pure[F]
        sortedPlayers <- initChooseDealer(players)
        dealer = sortedPlayers.head
        smallBlindPlayer = sortedPlayers.tail.head
        largeBlindPlayer = sortedPlayers.tail.tail.head
        updTable = table.copy(isActive = false)
      } yield (updTable, Game(UUID.randomUUID(), dealer, Blind(smallBlindPlayer), Blind(largeBlindPlayer), players = sortedPlayers))
    }

    override def smallBlind(gameId: UUID, userId: UUID, bet: BigDecimal): F[Blind] = for {
      gamePlayer <- validate(gameId, userId)
      (game, _) = gamePlayer
      blind = game.smallBlind.copy(bet = bet, wasDone = true)
      updGame = game.copy(bank = game.bank + bet, smallBlind = blind)
      _ <- userService.updateBudget(userId, Budget(-bet))
      _ <- gameWarehouse.put(updGame.id, updGame)
    } yield blind

    override def largeBlind(gameId: UUID, userId: UUID, bet: BigDecimal): F[Blind] = for {
      gamePlayer <- validate(gameId, userId)
      (game, _) = gamePlayer
      blind = game.largeBlind.copy(bet = bet, wasDone = true)
      updGame = game.copy(bank = game.bank + bet, largeBlind = blind)
      _ <- userService.updateBudget(userId, Budget(-bet))
      _ <- gameWarehouse.put(updGame.id, updGame)
      _ <- initializeCards(game)
    } yield blind

    override def opening(gameId: UUID): F[(UsernameID, Combination)] = for {
      game <- validateGame(gameId)
      board = game.board
      userCombination = game.playerGames.map(game => (game.player, combination(TexasCase(board, game.hand))))
      sorted = userCombination.sortBy(_._2)
    } yield sorted.last

    private[game] def combination(texasCase: TexasCase): Combination =
      RoyalFlush(texasCase).getOrElse(
        StraightFlush(texasCase).getOrElse(
          Straight(texasCase).getOrElse(
            FourOfAKind(texasCase).getOrElse(
              FullHouse(texasCase).getOrElse(
                Flush(texasCase).getOrElse(
                  ThreeOfKind(texasCase).getOrElse(
                    TwoPairs(texasCase).getOrElse(
                      Pair(texasCase).getOrElse(
                        HighCard(texasCase)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

    override def playerHand(gameId: UUID, userId: UUID): F[Hand] = for {
      gamePlayer <- validate(gameId, userId)
      (game, _) = gamePlayer
      handOpt <- game.playerGames.filter(_.player.id == userId).map(_.hand).headOption.pure[F]
      hand <- handOpt match {
        case None => Sync[F].raiseError(UserNotFound)
        case Some(x) => x.pure[F]
      }
    } yield hand

    // TODO: Implement it
    // It should restart a circle
    override def raise(gameId: UUID, userId: UUID, bet: BigDecimal): F[Unit] = call(gameId, userId, bet)

    override def call(gameId: UUID, userId: UUID, bet: BigDecimal): F[Unit] = for {
      gamePlayer <- validate(gameId, userId)
      (game, player) = gamePlayer
      playerUpd = player.copy(user = player.user.copy(budget = player.user.budget - bet))
      playerGames = game.playerGames.map(x => if (x.player.id == userId) {
        x.copy(finish = true)
      } else {
        x
      })
      _ <- sessionWarehouse.put(playerUpd.user.id, playerUpd)
      gameCopy = game.copy(bank = game.bank + bet, playerGames = playerGames)
      gameUpd = if (gameCopy.playerGames.forall(_.finish)) {
        if (gameCopy.state == Preflop) gameCopy.copy(state = Flop)
        else if (gameCopy.state == Flop) gameCopy.copy(state = Turn)
        else gameCopy.copy(state = Reaver)
      } else gameCopy
      _ <- gameWarehouse.put(gameUpd.id, gameUpd)
    } yield ()

    override def check(gameId: UUID, userId: UUID): F[Unit] = for {
      gamePlayer <- validate(gameId, userId)
      (game, _) = gamePlayer
      playerGames = game.playerGames.map(x => if (x.player.id == userId) {
        x.copy(finish = true)
      } else {
        x
      })
      gameCopy = game.copy(playerGames = playerGames)
      gameUpd = if (gameCopy.playerGames.forall(_.finish)) {
        if (gameCopy.state == Flop) gameCopy.copy(state = Turn)
        else gameCopy.copy(state = Reaver)
      } else gameCopy
      _ <- gameWarehouse.put(gameUpd.id, gameUpd)
    } yield ()

    override def turn(gameId: UUID): F[Unit] = openBoardCard(gameId, Flop)

    override def reaver(gameId: UUID): F[Unit] = openBoardCard(gameId, Reaver)

    private def openBoardCard(gameId: UUID, expectedGameState: GameState): F[Unit] = for {
      game <- validateGame(gameId)
      _ <- if (game.state != expectedGameState) {
        // Deactivated due to testing purpose
        //Sync[F].raiseError(WrongGameState(game.state))
        ().pure[F]
      } else ().pure[F]
      deckCard <- Deck.retrieveCard[F](game.deck)
      (updDeck, card) = deckCard
      updBoard = game.board.copy(value = game.board.value.appended(card))
      updGame = game.copy(board = updBoard, deck = updDeck)
      _ <- gameWarehouse.put(updGame.id, updGame)
    } yield ()

    private def initializeCards(game: Game): F[Unit] = for {
      deckBoard <- Deck.retrieveTexasBoard[F](game.deck)
      (deck, board) = deckBoard
      deckPlayerGames <- game.players.foldLeft[F[(Deck, List[PlayerGame])]]((deck, List.empty[PlayerGame]).pure[F]) {
        case (acc, player) => for {
          deckGamesPlayer <- acc
          (accDeck, accPlayerGames) = deckGamesPlayer
          texasHoldemHand <- Deck.retrieveTexasHand[F](accDeck)
          (holdemDeck, holdemHand) = texasHoldemHand
        } yield (holdemDeck, PlayerGame(player, holdemHand) :: accPlayerGames)
      }
      (updDeck, playerGames) = deckPlayerGames
      updGame = game.copy(board = board, playerGames = playerGames.reverse, deck = updDeck)
      _ <- gameWarehouse.put(updGame.id, updGame)
    } yield ()

    private def validate(gameId: UUID, userId: UUID): F[(Game, UserSession)] = for {
      game <- validateGame(gameId)
      optPlayer <- sessionWarehouse.get(userId)
      player <- optPlayer match {
        case None => Sync[F].raiseError(UserNotFound)
        case Some(x) => x.pure[F]
      }
      _ <- if (!game.players.exists(_.id == userId)) {
        Sync[F].raiseError(UserNotInGame)
      } else {
        ().pure[F]
      }
    } yield (game, player)

    private def validateGame(gameId: UUID): F[Game] = for {
      optGame <- gameWarehouse.get(gameId)
      game <- optGame match {
        case None => Sync[F].raiseError(GameNotFound)
        case Some(x) => x.pure[F]
      }
    } yield game
  }.pure[F]
}
