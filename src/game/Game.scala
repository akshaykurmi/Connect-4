package game

import game.Disc.{Disc, RED, YELLOW}

class Game {
    
    def makeMove(board: Board, column: Int, disc: Disc): Board = (board, column) match {
        case (b, c) if b isColumnFull c => b
        case (b, c) if b isColumnValid c =>
            Board(b.rows, b.columns, (disc, b nextPositionInColumn c) :: b.positions)
        case (b, _) => b
    }
    
    def currentPlayer(board: Board): Disc = board match {
        case b if b isEmpty => BLUE
        case b if b.count(BLUE) > b.count(YELLOW) => YELLOW
        case _ => BLUE
    }
    
    def hasWon(board: Board, disc: Disc): Boolean = {
        Position check4InARow (board getPositionsWith disc)
    }
    
    def isGameOver(board: Board): Boolean = {
        hasWon(board, BLUE) || hasWon(board, YELLOW) || (board isFull)
    }
}
