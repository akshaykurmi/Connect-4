package game

import game.Disc.Disc


object Disc extends Enumeration {
    type Disc = Value
    val RED = Value("RED")
    val YELLOW = Value("YELLOW")
}


case class Position(x: Int, y: Int)


case class Board(rows: Int, columns: Int, positions: List[(Disc, Position)])
