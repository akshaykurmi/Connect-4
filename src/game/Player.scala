package game

abstract class Player(name: String) {
    def getMove: Int
    override def toString: String = name
}
