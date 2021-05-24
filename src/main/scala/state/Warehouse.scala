package edu.agus.poker
package state

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._

import java.util.UUID

trait Warehouse[A, F[_]] {
  def get(id: UUID): F[Option[A]]

  def put(id: UUID, storageObject: A): F[Unit]

  def remove(id: UUID): F[Unit]

  def entries: F[Map[String, A]]
}

object Warehouse {
  def of[A, F[_] : Sync]: F[Warehouse[A, F]] = {
    Ref.of(Map.empty[String, A]).map { state =>
      new Warehouse[A, F] {
        override def get(itemId: UUID): F[Option[A]] = for {
          items <- state.get
          item = items.get(itemId.toString)
        } yield item

        override def put(itemId: UUID, item: A): F[Unit] = state.update { items =>
          items + (itemId.toString -> item)
        }

        override def remove(itemId: UUID): F[Unit] = state.update { items =>
          items.removed(itemId.toString)
        }

        override def entries: F[Map[String, A]] = state.get
      }
    }
  }
}
