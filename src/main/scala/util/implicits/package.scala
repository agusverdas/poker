package edu.agus.poker
package util

import doobie.util.meta.Meta

import java.util.UUID

package object implicits {
  implicit val uuidMeta: Meta[UUID] = Meta[String].timap(UUID.fromString)(_.toString)
}
