package game

import game.Disc.Disc


object Disc extends Enumeration {
    type Disc = Value
    val RED = Value("RED")
    val YELLOW = Value("YELLOW")
}


case class Position(x: Int, y: Int)


case class Board(rows: Int, columns: Int, positions: List[(Disc, Position)]) {
    def nextPositionInColumn(column: Int): Position =
        Position((positions count {_._2.y == column}) + 1, column)
    
    def count(disc: Disc): Int = positions count {_._1 == disc}
    
    def isEmpty: Boolean = positions isEmpty
    
    def isColumnFull(column: Int): Boolean =
        (positions count {_._2.y == column}) == rows
    
    def isColumnValid(column: Int): Boolean =
        0 until columns contains column
}
