package ui

import game.Disc.{BLUE, Disc, YELLOW}
import game._
import ai.{MinimaxPlayer, RLPlayer}

object ConsoleApp {
    
    def stringifyBoard(board: Board): String = {
        "\t" + ((for (r <- 0 to board.rows; c <- 0 to board.columns) yield (r, c)).reverse map {
            case (a, b) if (board.positions map {_._2}).contains(Position(a, b)) =>
                stringifyDisc(board.positions((board.positions map {_._2}).indexOf(Position(a, b)))._1)
            case (_, b) if b == board.columns => "\n"
            case (a, b) if a == 0 => Console.WHITE + b
            case (_, _) => Console.WHITE + 0x00B7.toChar
        } mkString "\t")
    }
    
    def stringifyDisc(disc: Disc): String = disc match {
        case BLUE => Console.BLUE + 0x2B24.toChar
        case YELLOW => Console.YELLOW + 0x2B24.toChar
    }
    
    def play(board: Board): Option[Player] = {
        println(stringifyBoard(board))
        if (board.isGameOver) board.winningPlayer
        else play(board.makeMove(board.currentPlayer.getMove(board), board.currentDisc))
    }
    
    def main(args: Array[String]): Unit = {
        print("Enter Player 1's name : ")
        val playerBlue = RLPlayer.createRLPlayer(scala.io.StdIn.readLine(), "rlplayertest.zip")
        print("Enter Player 2's name : ")
        val playerYellow = new HumanConsolePlayer(scala.io.StdIn.readLine())
        
        val player = play(Board(6, 7, Nil, playerBlue, playerYellow))
        
        if (player.isDefined) println(player.get + " has won!")
        else println("The match is a draw")
    }
    
}


class HumanConsolePlayer(name: String) extends Player(name) {
    def prompt: String = Console.WHITE + name + ", select a column to drop the disc in : "
    
    override def getMove(board: Board): Int = {
        print(prompt)
        scala.io.StdIn.readInt()
    }
}
