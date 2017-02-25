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
        case b if b isEmpty => RED
        case b if b.count(RED) > b.count(YELLOW) => YELLOW
        case _ => RED
    }
    
}
