package game

import game.Disc.{Disc, RED, YELLOW}

class Game {
    
    def makeMove(board: Board, position: Position, disc: Disc): Board = (board, position) match {
        case (b, p) if b contains p => b
        case (b, p) if p isValid(b.rows, b.columns) => Board(b.rows, b.columns, (disc, position) :: b.positions)
        case (_, _) => board
    }
    
    def currentPlayer(board: Board): Disc = board match {
        case b if b isEmpty => RED
        case b if b.count(RED) > b.count(YELLOW) => YELLOW
        case _ => RED
    }
    
}
