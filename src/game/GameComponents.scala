package game

import game.Disc.Disc


object Disc extends Enumeration {
    type Disc = Value
    val RED = Value("RED")
    val YELLOW = Value("YELLOW")
}


case class Position(x: Int, y: Int)

case object Position {
    
    private def areConsecutive(numbers: List[Int]): Boolean =
        numbers.length == ((((numbers sorted) sliding 2) map(a => a.last - a.head)) count {_ == 1})
    
    private def areHorizontal(positions: List[Position]): Boolean =
        (positions forall {_.y == positions.head.y}) &&
            areConsecutive(positions map {_.x})
    
    private def areVertical(positions: List[Position]): Boolean =
        (positions forall {_.x == positions.head.x}) &&
            areConsecutive(positions map {_.y})
    
    private def areDiagonal(positions: List[Position]): Boolean =
        areConsecutive(positions map {_.y}) &&
            areConsecutive(positions map {_.x})
    
    private def checkInARow(positions: List[Position]): Boolean =
        areHorizontal(positions) || areVertical(positions) || areDiagonal(positions)
    
    def check4InARow(positions: List[Position]): Boolean =
        ((positions combinations 4) toList)
            .foldLeft(false)((bool, list) => bool || checkInARow(list))
}


case class Board(rows: Int, columns: Int, positions: List[(Disc, Position)]) {
    def isFull: Boolean =
        positions.length == rows * columns
    
    def getPositionsWith(disc: Disc): List[Position] =
        positions filter {_._1 == disc} map {_._2}
    
    def nextPositionInColumn(column: Int): Position =
        Position((positions count {_._2.y == column}) + 1, column)
    
    def count(disc: Disc): Int =
        positions count {_._1 == disc}
    
    def isEmpty: Boolean =
        positions isEmpty
    
    def isColumnFull(column: Int): Boolean =
        (positions count {_._2.y == column}) == rows
    
    def isColumnValid(column: Int): Boolean =
        0 until columns contains column
}
