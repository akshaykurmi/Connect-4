package ai

import java.io.File
import java.nio.file.{Files, Paths}

import game.{Board, Player}
import org.deeplearning4j.nn.api.OptimizationAlgorithm
import org.deeplearning4j.nn.conf.layers.{DenseLayer, OutputLayer}
import org.deeplearning4j.nn.conf.{MultiLayerConfiguration, NeuralNetConfiguration, Updater}
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.weights.WeightInit
import org.deeplearning4j.optimize.listeners.ScoreIterationListener
import org.deeplearning4j.util.ModelSerializer
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.dataset.DataSet
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.lossfunctions.LossFunctions.LossFunction

import scala.collection.JavaConverters._

// TODO: Use discount factor
// TODO: Exploration factor
// TODO: Experience replay
// TODO: Convolution net

class RLPlayer(name: String, model: MultiLayerNetwork) extends Player(name) {
    override def getMove(board: Board): Int = {
        val zeros = Nd4j.zeros(6, 7)
        board.positions.foreach(pos => {
            if (pos._1 == board.currentDisc) zeros.putScalar(Array(pos._2.x - 1, pos._2.y), 1)
            else zeros.putScalar(Array(pos._2.x - 1, pos._2.y), -1)
        })
        val predictions = model.output(Nd4j.toFlattened(zeros))
//        println(predictions)
        val sorted = Nd4j.sortWithIndices(predictions, 1, false)
        val sortedIndices = sorted(0)
//        println(sorted(0))
//        println(sorted(1))
//        println()
        var move = 0
        for (i <- 0 until predictions.length()) {
            val column = sortedIndices.getDouble(i).toInt
            if (board.isColumnValid(column)) {
                move = column
                return move
            }
        }
        move
    }
}

object RLPlayer {
    def createRLPlayer(name: String, serializedModelPath: String): RLPlayer = {
        val modelFile: File = new File(serializedModelPath)
        val model = ModelSerializer.restoreMultiLayerNetwork(modelFile, true)
        new RLPlayer(name, model)
    }
}


object TrainRLPlayer {
    private def play(board: Board): (Option[Player], List[(Board, Int)], List[(Board, Int)]) = {
        def recur(board: Board, blue_history: List[(Board, Int)], yellow_history: List[(Board, Int)]): (Option[Player], List[(Board, Int)], List[(Board, Int)]) = {
            if (board.isGameOver) (board.winningPlayer, blue_history, yellow_history)
            else {
                val move = board.currentPlayer.getMove(board)
                val nextBoard = board.makeMove(move, board.currentDisc)
                if (board.currentPlayer == board.playerBlue) recur(nextBoard, (board, move) :: blue_history, yellow_history)
                else recur(nextBoard, blue_history, (board, move) :: yellow_history)
            }
        }
        
        recur(board, Nil, Nil)
    }
    
    def train(epochs: Int, serializedModelPath: String): Unit = {
        val model = getModel(serializedModelPath)
        for (i <- 1 to epochs) {
            println("Epoch : " + i)
            val playerBlue = new RLPlayer("RL_Blue", model)
            val playerYellow = new RLPlayer("RL_Yellow", model)
            
            def recur(count: Int, histories: List[(Option[Player], List[(Board, Int)], List[(Board, Int)])]): List[(Option[Player], List[(Board, Int)], List[(Board, Int)])] = {
                if (count == 0) histories
                else {
                    val (winner, blue_boards, yellow_boards) = play(Board(6, 7, Nil, playerBlue, playerYellow))
//                    println("Epoch : " + i + " | Iterations remaining : " + count + " | " + winner + " won")
                    recur(count - 1, (winner, blue_boards, yellow_boards) :: histories)
                }
            }
            
            val histories = recur(100, Nil)
            histories foreach (history => {
                val (winner, blue_boards, yellow_boards) = history
                if (winner.isDefined) {
                    def fit_model(boards: List[(Board, Int)], reward: Double, discountFactor: Double): Unit = {
                        val data = boards.map(board => {
                            val zeros = Nd4j.zeros(6, 7)
                            board._1.positions.foreach(pos => {
                                if (pos._1 == board._1.currentDisc) zeros.putScalar(Array(pos._2.x - 1, pos._2.y), 1)
                                else zeros.putScalar(Array(pos._2.x - 1, pos._2.y), -1)
                            })
                            Nd4j.toFlattened(zeros)
                        }).asJava
                        val labels = boards.zipWithIndex.map(board => {
                            val zeros = Nd4j.zeros(7)
//                            reward * math.pow(discountFactor, board._2 + 1)
                            zeros.putScalar(board._1._2, 1)
                            zeros
                        }).asJava
                        val featureMask = boards.map(board => {
                            Nd4j.ones(6 * 7)
                        }).asJava
                        val labelMask = boards.zipWithIndex.map(board => {
                            val ones = Nd4j.ones(7)
                            ones.putScalar(board._1._2, reward * math.pow(discountFactor, board._2))
                            ones
                        }).asJava
                        val data_new = Nd4j.create(data, Array(boards.length, 42))
                        val labels_new = Nd4j.create(labels, Array(boards.length, 7))
                        val featureMask_new = Nd4j.create(featureMask, Array(boards.length, 42))
                        val labelMask_new = Nd4j.create(labelMask, Array(boards.length, 7))
                        println(data_new.shape().toList)
                        println(labels_new.shape().toList)
                        println(featureMask_new.shape().toList)
                        println(labelMask_new.shape().toList)
//                        println(data_new)
//                        println(labels_new)
//                        println()
//                        model.fit(new DataSet(data_new, labels_new, null, labelMask_new))
                        
                    }
                    
                    if (winner.get == playerBlue) {
                        fit_model(blue_boards, 1, 0.90)
                        fit_model(yellow_boards, -1, 0.90)
                    }
                    else {
                        fit_model(blue_boards, -1, 0.90)
                        fit_model(yellow_boards, 1, 0.90)
                    }
                }
            })
        }
        ModelSerializer.writeModel(model, new File(serializedModelPath), true)
    }
    
    def getModel(serializedModelPath: String): MultiLayerNetwork = {
        if (Files.exists(Paths.get(serializedModelPath))) {
            print("Reloading model")
            val modelFile: File = new File(serializedModelPath)
            ModelSerializer.restoreMultiLayerNetwork(modelFile, true)
        }
        else {
            val model: MultiLayerNetwork = new MultiLayerNetwork(getNetworkConfiguration)
            model.init()
            model.setListeners(new ScoreIterationListener(1))
            model
        }
    }
    
    private def getNetworkConfiguration: MultiLayerConfiguration = {
        new NeuralNetConfiguration.Builder()
            .seed(100)
            .iterations(1)
            .regularization(true).l2(0.001)
            .learningRate(0.1)
            .weightInit(WeightInit.XAVIER)
            .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
            .updater(Updater.NESTEROVS).momentum(0.9)
            .list()
            .layer(0, new DenseLayer.Builder()
                .nIn(6 * 7)
                .nOut(400)
                .activation(Activation.RELU)
                .weightInit(WeightInit.XAVIER)
                .build())
            .layer(1, new OutputLayer.Builder(LossFunction.NEGATIVELOGLIKELIHOOD)
                .nIn(400)
                .nOut(7)
                .activation(Activation.SOFTMAX)
                .weightInit(WeightInit.XAVIER)
                .build())
            .pretrain(false).backprop(true)
            .build()
    }
    
    def main(args: Array[String]): Unit = {
        train(1000, "rlplayertest.zip")
    }
}
