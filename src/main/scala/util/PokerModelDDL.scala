package edu.agus.poker
package util

object PokerModelDDL {
  val UsersTable = "users"

  val CreateUserTable =
    s"""CREATE TABLE $UsersTable (
       |  id UUID PRIMARY KEY,
       |  name VARCHAR(16) NOT NULL,
       |  password VARCHAR(100) NOT NULL,
       |  budget DECIMAL(20, 2) DEFAULT 0,
       |  createdAt DATE DEFAULT NOW() NOT NULL,
       |  isAdmin BOOLEAN DEFAULT FALSE);""".stripMargin

}
