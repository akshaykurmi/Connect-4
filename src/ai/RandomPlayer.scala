package ai

import game.{Board, Player}

import scala.util.Random


class RandomPlayer(name: String) extends Player(name) {
    override def getMove(board: Board): Int = Random.nextInt(7)
}
