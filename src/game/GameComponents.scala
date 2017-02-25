package game

import game.Disc.Disc


object Disc extends Enumeration {
    type Disc = Value
    val RED = Value("RED")
    val YELLOW = Value("YELLOW")
}


case class Position(x: Int, y: Int) {
    def isValid(rows: Int, columns: Int): Boolean =
        (0 until rows contains x) && (0 until columns contains y)
}


case class Board(rows: Int, columns: Int, positions: List[(Disc, Position)]) {
    def count(disc: Disc): Int = positions count {
        _._1 == disc
    }
    
    def isEmpty: Boolean = positions isEmpty
    
    def contains(position: Position): Boolean =
        positions forall {
            _._2 != position
        }
}
