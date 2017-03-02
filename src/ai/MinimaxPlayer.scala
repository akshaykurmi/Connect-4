package ai

import game.Disc.Disc
import game.{Board, Disc, Player}

import scala.util.Random


class MinimaxPlayer(name: String) extends Player(name) {
    
    override def getMove(board: Board): Int =
        Random.shuffle({
            board.availableMoves.map{
                move => move -> minimax(board.currentDisc, board.makeMove(move, board.currentDisc))
            }
        }).maxBy(_._2)._1
    
    private def minimax(myDisc: Disc, board: Board): Int =
        minimize(myDisc, board, 0, Integer.MIN_VALUE, Integer.MAX_VALUE)
    
    private def minimize(myDisc: Disc, board: Board, depth: Int, alpha: Int, beta: Int): Int = {
        if (board.isGameOver || depth == 5) return score(board, myDisc, depth)
        var newBeta = beta
        board.availableMoves.foreach(move => {
            val newState = board.makeMove(move, board.currentDisc)
            newBeta = math.min(beta, maximize(myDisc, newState, depth + 1, alpha, newBeta))
            if (alpha >= newBeta) return alpha
        })
        newBeta
    }
    
    private def maximize(myDisc: Disc, board: Board, depth: Int, alpha: Int, beta: Int): Int = {
        if (board.isGameOver || depth == 5) return score(board, myDisc, depth)
        var newAlpha = alpha
        board.availableMoves.foreach(move => {
            val newState = board.makeMove(move, board.currentDisc)
            newAlpha = math.max(newAlpha, minimize(myDisc, newState, depth + 1, newAlpha, beta))
            if (newAlpha >= beta) return beta
        })
        newAlpha
    }
    
    def score(board: Board, myDisc: Disc, depth: Int): Int =
        if (board.hasWon(myDisc)) 100 - depth
        else if (board.hasWon(Disc.otherDisc(myDisc))) depth - 100
        else 0
}
