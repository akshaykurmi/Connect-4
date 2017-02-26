package ai

import game.Player
import scala.util.Random

class RandomPlayer(name: String) extends Player(name) {
    override def getMove: Int = Random.nextInt(7)
}
