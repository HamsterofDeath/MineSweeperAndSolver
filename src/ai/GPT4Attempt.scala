package ai

import java.awt.Point
import Sweeper._

class GPT4Attempt extends MineFinder {

  private var mineField: MineField = _

  private val unvisited = scala.collection.mutable.HashSet[Point]()

  override def start(field: MineField): Unit = {
    mineField = field
    val dimension = mineField.getFieldSize()
    for {
      x <- 0 until dimension.width
      y <- 0 until dimension.height
      point = new Point(x, y)
    } unvisited += point

    var startingPoint: Option[Point] = Some(mineField.getStartingPoint())
    while(startingPoint.isDefined) {
      solve(startingPoint.get)
      unvisited -= startingPoint.get
      startingPoint = findNextStartingPoint()
      startingPoint.foreach { mineField.setFlag(_,MineFieldFlag.OPEN)}
    }
  }
  override def stop(): Unit = {
    mineField = null
  }

  override def getIdentification(): String = "GPT4Attempt"

  private def solve(start: Point) = {
    val toVisit = scala.collection.mutable.HashSet(start)
    val visited = scala.collection.mutable.HashSet(start)
    var changesMade: Boolean = false

    while (toVisit.nonEmpty) {
      val current = toVisit.head
      toVisit.remove(current)

      val neighbors = getNeighbors(current)
      val value = mineField.getValue(current)

      if (value >= 0) {
        val unopenedNeighbors = neighbors.filter(p => mineField.getFlag(p) == MineFieldFlag.NOTSET)
        val flaggedNeighbors = neighbors.filter(p => mineField.getFlag(p) == MineFieldFlag.FLAGGED)
        if (value == unopenedNeighbors.size + flaggedNeighbors.size) {
          unopenedNeighbors.foreach(p => {
            mineField.setFlag(p, MineFieldFlag.FLAGGED)
            changesMade = true
          })
        } else if (value == flaggedNeighbors.size) {
          unopenedNeighbors.foreach { p =>
            if(mineField.getFlag(p) != MineFieldFlag.OPEN) {
              changesMade = true
            }
            mineField.setFlag(p, MineFieldFlag.OPEN)
            if (!visited.contains(p)) {
              visited.add(p)
              toVisit.add(p)
            }
          }
        }
      }
      unvisited -= current
    }
  }

  private def findNextStartingPoint(): Option[Point] = {
    unvisited.find(isSafePoint)
  }

  private def isSafePoint(point: Point): Boolean = {
    mineField.getFlag(point) match {
      case MineFieldFlag.NOTSET =>
        val neighbors = getNeighbors(point)
        neighbors.exists(n => {
          val nValue = mineField.getValue(n)
          if (nValue > 0) {
            val nNeighbors = getNeighbors(n)
            val nFlaggedNeighbors = nNeighbors.count(nn => mineField.getFlag(nn) == MineFieldFlag.FLAGGED)
            nValue == nFlaggedNeighbors
          } else {
            false
          }
        })
      case _ => false
    }
  }

  private def getNeighbors(point: Point): Seq[Point] = {
    val dimension = mineField.getFieldSize()
    for {
      dx <- -1 to 1
      dy <- -1 to 1
      x = point.x + dx
      y = point.y + dy
      if x >= 0 && x < dimension.width
      if y >= 0 && y < dimension.height
    } yield new Point(x, y)
  }
}
