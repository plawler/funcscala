package funcscala.chapterone

/**
 * Created by paullawler on 2/19/15.
 */

case class Player(name: String, score: Int)

object ChapterOne extends App {

  def printWinner(p: Player) = println(p.name + " is the winner")
  def declareWinner(players: Seq[Player]) = printWinner(players.reduceLeft(winner))
  def winner(p1: Player, p2: Player): Player = if (p1.score > p2.score) p1 else p2

  val players = List(Player("Sue", 7), Player("Bob", 8), Player("Bill", 10))

  declareWinner(players)

}
