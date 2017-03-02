package game

import game.Disc.Disc
import game.Disc.BLUE
import game.Disc.YELLOW


object Disc extends Enumeration {
    type Disc = Value
    val BLUE = Value("B")
    val YELLOW = Value("Y")
    
    def otherDisc(disc: Disc): Disc = disc match {
        case BLUE => YELLOW
        case YELLOW => BLUE
    }
}


case class Position(x: Int, y: Int)

case object Position {
    
    private def areConsecutive(numbers: List[Int]): Boolean =
        (numbers.length - 1 == (((numbers sliding 2) map(a => a.last - a.head)) count {_ == 1})) ||
            numbers.length - 1 == (((numbers sliding 2) map(a => a.last - a.head)) count {_ == -1})
    
    private def areHorizontal(positions: List[Position]): Boolean =
        (positions forall {_.y == positions.head.y}) &&
            areConsecutive((positions map {_.x}) sorted)
    
    private def areVertical(positions: List[Position]): Boolean =
        (positions forall {_.x == positions.head.x}) &&
            areConsecutive((positions map {_.y}) sorted)
    
    private def areDiagonal(positions: List[Position]): Boolean =
        areConsecutive((positions sortBy {_.x}) map {_.x}) &&
            areConsecutive((positions sortBy {_.x}) map {_.y})
    
    private def checkInARow(positions: List[Position]): Boolean =
        areHorizontal(positions) || areVertical(positions) || areDiagonal(positions)
    
    def check4InARow(positions: List[Position]): Boolean =
        ((positions combinations 4) toList)
            .foldLeft(false)((bool, list) => bool || checkInARow(list))
}


case class Board(rows: Int, columns: Int, positions: List[(Disc, Position)],
                 playerBlue: Player, playerYellow: Player) {
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
        !isColumnFull(column) && (0 until columns contains column)
    
    def availableMoves: List[Int] =
        ((0 until columns) filter (!isColumnFull(_))) toList
    
    def currentDisc: Disc =
        if (count(BLUE) > count(YELLOW)) BLUE else YELLOW
    
    def hasWon(disc: Disc): Boolean =
        Position check4InARow getPositionsWith(disc)
    
    def isGameOver: Boolean =
        hasWon(BLUE) || hasWon(YELLOW) || isFull
    
    def winningPlayer: Option[Player] = {
        if (hasWon(BLUE)) Some(playerBlue)
        else if (hasWon(YELLOW)) Some(playerYellow)
        else None
    }
    
    def currentPlayer: Player = currentDisc match {
        case BLUE => playerBlue
        case YELLOW => playerYellow
    }
    
    def makeMove(column: Int, disc: Disc): Board =
        if (!isColumnValid(column)) this
        else Board(rows, columns, (disc, nextPositionInColumn(column)) :: positions, playerBlue, playerYellow)
}
