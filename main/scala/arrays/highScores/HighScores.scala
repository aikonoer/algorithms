package arrays.highScores

/**
  * Created by brianmomongan on 4/8/16.
  */
class HighScores {

}

case class GameEntry(name: String, score: Int)

case class ScoreBoard(private val capacity: Int) {
  private var entries: Int = 0
  var gameBoard: Array[GameEntry] = new Array[GameEntry](capacity)

  def +=(gameEntry: GameEntry) = {
    val entry = gameEntry.score


    if (entries < capacity)
      gameBoard(entries) = gameEntry
    entries += 1
  }
}

object Main extends App {

  val scoreBoard = ScoreBoard(10)
  scoreBoard.+=(GameEntry("Diane0", 40))
  scoreBoard.+=(GameEntry("Diane1", 41))
  scoreBoard.+=(GameEntry("Diane2", 42))
  scoreBoard.+=(GameEntry("Diane3", 43))
  scoreBoard.+=(GameEntry("Diane4", 44))
  scoreBoard.+=(GameEntry("Diane5", 45))
  scoreBoard.+=(GameEntry("Diane6", 46))
  scoreBoard.+=(GameEntry("Diane7", 47))
  scoreBoard.+=(GameEntry("Diane8", 48))
  scoreBoard.+=(GameEntry("Diane9", 49))

  scoreBoard.gameBoard map (entry => println(entry.name))
}









