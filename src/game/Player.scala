package game

abstract class Player(name: String) {
    def getMove(board: Board): Int
    override def toString: String = name
}
