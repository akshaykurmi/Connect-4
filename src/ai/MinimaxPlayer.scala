package ai

import game.Disc.Disc
import game.{Board, Disc, Player}

import scala.util.Random


class MinimaxPlayer(name: String) extends Player(name) {
    
    override def getMove(board: Board): Int =
        Random.shuffle(
            board.availableMoves.map {
                move => move -> minimax(board.currentDisc, board.makeMove(move, board.currentDisc))
            }
        ).maxBy(_._2)._1
    
    private def minimax(myDisc: Disc, board: Board): Int =
        minimize(myDisc, board, 0, Integer.MIN_VALUE, Integer.MAX_VALUE)
    
    private def minimize(myDisc: Disc, board: Board, depth: Int, alpha: Int, beta: Int): Int = {
        if (board.isGameOver || depth == 6) return score(board, myDisc, depth)
        board.availableMoves.foldLeft(beta)((b, move) => {
            val newBoard = board.makeMove(move, board.currentDisc)
            val newBeta = math.min(b, maximize(myDisc, newBoard, depth + 1, alpha, b))
            if (alpha >= newBeta) return alpha
            newBeta
        })
    }
    
    private def maximize(myDisc: Disc, board: Board, depth: Int, alpha: Int, beta: Int): Int = {
        if (board.isGameOver || depth == 6) return score(board, myDisc, depth)
        board.availableMoves.foldLeft(alpha)((a, move) => {
            val newBoard = board.makeMove(move, board.currentDisc)
            val newAlpha = math.max(a, minimize(myDisc, newBoard, depth + 1, a, beta))
            if (newAlpha >= beta) return beta
            newAlpha
        })
    }
    
    def score(board: Board, myDisc: Disc, depth: Int): Int =
        if (board.hasWon(myDisc)) 100 - depth
        else if (board.hasWon(Disc.otherDisc(myDisc))) depth - 100
        else 0
}
