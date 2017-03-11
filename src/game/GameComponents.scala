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
    
    def horizontalPositions(position: Position): List[Position] =
        List(Position(position.x+1, position.y),
             Position(position.x+2, position.y),
             Position(position.x+3, position.y))
    
    def verticalPositions(position: Position): List[Position] =
        List(Position(position.x, position.y+1),
             Position(position.x, position.y+2),
             Position(position.x, position.y+3))
    
    def majorDiagonalPositions(position: Position): List[Position] =
        List(Position(position.x+1, position.y+1),
             Position(position.x+2, position.y+2),
             Position(position.x+3, position.y+3))
    
    def minorDiagonalPositions(position: Position): List[Position] =
        List(Position(position.x-1, position.y-1),
             Position(position.x-2, position.y-2),
             Position(position.x-3, position.y-3))
    
    def check4InARow(positions: List[Position]): Boolean =
        positions.foldLeft(false)((bool, position) => {
            bool ||
            horizontalPositions(position).forall(p => positions contains p) ||
            verticalPositions(position).forall(p => positions contains p) ||
            majorDiagonalPositions(position).forall(p => positions contains p) ||
            minorDiagonalPositions(position).forall(p => positions contains p)
        })
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
        if (count(BLUE) == count(YELLOW)) BLUE else YELLOW
    
    def hasWon(disc: Disc): Boolean =
        Position check4InARow getPositionsWith(disc)
    
    def isGameOver: Boolean =
        hasWon(BLUE) || hasWon(YELLOW) || isFull
    
    def winningPlayer: Option[Player] =
        if (hasWon(BLUE)) Some(playerBlue)
        else if (hasWon(YELLOW)) Some(playerYellow)
        else None
    
    def currentPlayer: Player = currentDisc match {
        case BLUE => playerBlue
        case YELLOW => playerYellow
    }
    
    def makeMove(column: Int, disc: Disc): Board =
        if (!isColumnValid(column)) this
        else Board(rows, columns, (disc, nextPositionInColumn(column)) :: positions, playerBlue, playerYellow)
}
