package ai

import java.awt.Point

import Sweeper.{MineField, MineFieldFlag, MineFinder}

import scala.collection.mutable

/**
  * Created by HamsterofDeath on 8/22/2016.
  */
class HoDSolve2016 extends MineFinder {

  import Utils._

  private var run = true

  override def stop() = {
    run = false
  }

  override def getIdentification = "HamsterofDeath Minefinder 0.7.1"

  private val UNKNOWN_STATE = -1

  def solve(myField: FieldData, nativeField: MineField): Unit = {
    var safeTodo = mutable.ArrayBuffer(myField.start)
    var unsafeTodo = mutable.ArrayBuffer.empty[Point]
    var expensiveTodo = mutable.ArrayBuffer.empty[Point]
    var giveUpHere = mutable.HashSet.empty[Point]
    var change = false
    while (safeTodo.nonEmpty || unsafeTodo.nonEmpty || expensiveTodo.nonEmpty) {
      def flagAsBomb(p:Point):Unit = {
        nativeField.setFlag(p, MineFieldFlag.FLAGGED)
        myField.informBomb(p)
        change = true
      }

      def openField(p: Point): Unit = {
        nativeField.setFlag(p, MineFieldFlag.OPEN)
        val bombCount = nativeField.getValue(p)
        myField.informSafe(p, bombCount)
        change = true
        if (bombCount == 0) {
          safeTodo ++= myField.around(p, withHidden = true, mutablePoints = false)
        } else {
          unsafeTodo ++= myField.around(p, withHidden = true, mutablePoints = false)
        }
      }

      if (safeTodo.nonEmpty) {
        val next = safeTodo.remove(safeTodo.size - 1)
        if (myField.isClosed(next)) {
          openField(next)
        }
      } else if (unsafeTodo.nonEmpty) {
        val next = unsafeTodo.remove(unsafeTodo.size-1)

        if (myField.isClosed(next)) {
          if (myField.mustBeSafe(next)) {
            openField(next)
          } else if (myField.mustBeBomb(next)) {
            flagAsBomb(next)
            unsafeTodo ++= myField.around(next, withHidden = true, mutablePoints = false)
          } else {
            expensiveTodo += next
          }
        }
      } else if (expensiveTodo.nonEmpty) {
        val next = expensiveTodo.remove(expensiveTodo.size-1)
        if (myField.isClosed(next)) {
          if (myField.mustBeSafe2(next)) {
            openField(next)
          }
        }

        if (expensiveTodo.isEmpty && change) {
          change = false
          val nextTries = myField.allClosedFields.filter { p =>
            myField.nextToOpen(p)
          }
          unsafeTodo ++= nextTries
        }
      }
    }

    println()
  }

  override def start(mineField: MineField) = {
    run = true
    val size = mineField.getFieldSize
    val meta = FieldData(size.width, size.height,
      mineField.getBombCount,
      mineField.getStartingPoint)

    solve(meta, mineField)
    println()
  }

  case class FieldData(width:Int, height:Int, bombs:Int, start:Point) {
    def nextToOpen(p: Point) = {
      around(p, withOpen = true, mutablePoints = true).nonEmpty
    }

    def allClosedFields = new Traversable[Point] {
      override def foreach[U](f: (Point) => U) = {
        for (x <- 0 until width;y <- 0 until height) {
          val p = new Point(x,y)
          if (isClosed(p)) {
            f(p)
          }
        }
      }
    }

    @inline def isClosed(where: Point): Boolean = !isOpen(where) &&
                                                  !bombDetected.at(where)

    @inline def isOpen(where:Point) = bombCounts.at(where) != UNKNOWN_STATE

    @inline def mustBeSafe(where: Point) = {
      around(where, withOpen = true, mutablePoints = true).exists { box =>
        remainingSurroundingBombs.at(box) == 0 && isOpen(box)
      }
    }

    @inline def pretendBomb(where: Point) = {
      informBomb(where)
    }

    @inline def rectangleAround(where: Point, range: Int) = {
      val minX = 0 max (where.x - range)
      val maxX = width min (where.x + range + 1)
      val minY = 0 max (where.y - range)
      val maxY = height min (where.y + range + 1)

      new Traversable[Point] {
        override def foreach[U](f: (Point) => U) = {
          val p = new Point
          for (x <- minX until maxX;y <- minY until maxY) {
            p.move(x, y)
            f(p)
          }
        }
      }
    }

    @inline def mustBeSafe2(where: Point) = {
      @inline def contradiction(p:Point) = {
        val reference = remainingSurroundingBombs.at(p)
        def possible = around(p, withHidden = true, mutablePoints = true).count { canIBeABomb =>
          !around(canIBeABomb, withOpen = true, mutablePoints = true).exists { checkIfIAmZero =>
            remainingSurroundingBombs.at(checkIfIAmZero) == 0
          }
        }
        reference > 0 && reference > possible
      }

      pretendBomb(where)
      val safe = rectangleAround(where, 3).exists(contradiction)
      unPretendBomb(where)
      safe
    }

    def dumpBombCounts = {
      (for (y <- 0 until height) yield {
        (for (x <- 0 until width) yield {
          val count = bombCounts(x)(y)
          val bomb = bombDetected(x)(y)
          if (bomb) "B" else if (count == -1) "?" else count
        }).mkString
      }).mkString("\n")
    }

    def dumpRemainingBombCounts = {
      (for (y <- 0 until height) yield {
        (for (x <- 0 until width) yield {
          val count = remainingSurroundingBombs(x)(y)
          val bomb = bombDetected(x)(y)
          if (bomb) "B" else if (isClosed(new Point(x,y))) "?" else count
        }).mkString
      }).mkString("\n")
    }

    @inline def mustBeBomb(where:Point) = {
      pretendSafe(where)
      val bomb = around(where, withOpen = true, mutablePoints = true).exists { box =>
        val possibleRemainingBombs = around(box, withHidden = true, mutablePoints = true).size
        possibleRemainingBombs < remainingSurroundingBombs.at(box)
      }
      unpretendSafe(where)
      bomb
    }

    @inline def pretendSafe(where:Point):Unit = {
      bombCounts(where.x)(where.y) = 0
    }

    @inline def unpretendSafe(where:Point):Unit = {
      bombCounts(where.x)(where.y) = UNKNOWN_STATE
    }

    @inline def allAround(where: Point) = around(where, withHidden = true, withOpen = true, mutablePoints = true)

    @inline def informBomb(where: Point): Unit = {
      remainingBombs -= 1
      unknown -= 1
      allAround(where).foreach { p =>
        remainingSurroundingBombs(p.x)(p.y) -= 1
      }
      bombDetected(where.x)(where.y) = true
    }

    @inline def unPretendBomb(where:Point):Unit = {
      remainingBombs += 1
      allAround(where).foreach { p =>
        remainingSurroundingBombs(p.x)(p.y) += 1
      }
      bombDetected(where.x)(where.y) = false
    }

    private var unknown = width * height
    private var remainingBombs = bombs

    @inline def informSafe(where: Point, bombCount: Int): Unit = {
      bombCounts(where.x)(where.y) = bombCount
      remainingSurroundingBombs(where.x)(where.y) += bombCount
      unknown -= 1
    }

    @inline def around(p: Point, withHidden: Boolean = false, withOpen: Boolean = false,
                       mutablePoints: Boolean): Traversable[Point] = {
      val withAll = withHidden && withOpen
      val t: Traversable[Point] = new Traversable[Point] {

        @inline def isInBoundsUnknown(p:Point) = inBounds(p) && isUnknown(p)

        @inline def inBounds(p:Point) = {
          p.x >= 0 &&
          p.y >= 0 &&
          p.x < width &&
          p.y < height
        }

        @inline def isInBoundsKnown(p: Point) = inBounds(p) && isKnown(p)
        @inline def isUnknown(p:Point) = isClosed(p)
        @inline def isKnown(p:Point) = bombCounts(p.x)(p.y) != UNKNOWN_STATE
        @inline def simple = notLeft && notRight && notTop && notBottom
        @inline def notLeft = p.x > 0
        @inline def left = !notLeft
        @inline def notRight = p.x < width - 1
        @inline def right = !notRight
        @inline def notTop = p.y > 0
        @inline def top = !notTop
        @inline def notBottom = p.y < height -1
        @inline def bottom = !notBottom

        @inline def checked[U](f:Point => U) = {
          allWithTest(f, if (withAll) inBounds else if (withHidden) isInBoundsUnknown else isInBoundsKnown)
        }

        @inline def unchecked[U](f:Point => U) = {
          if (withAll) {
            all(f)
          } else {
            allWithTest(f, if (withHidden) isUnknown else isKnown)
          }
        }

        @inline def all[U](f: (Point) => U) = {
          val mut = new Point(p.x - 1, p.y - 1)
          f(mut)
          mut.x += 1
          f(mut)
          mut.x += 1
          f(mut)
          mut.y += 1
          f(mut)
          mut.y += 1
          f(mut)
          mut.x -= 1
          f(mut)
          mut.x -= 1
          f(mut)
          mut.y -= 1
          f(mut)
        }

        @inline def allWithTest[U](f: (Point) => U, @inline test: Point => Boolean) = {
          val mut = new Point(p.x - 1, p.y - 1)
          if (test(mut)) f(mut)
          mut.x +=1
          if (test(mut)) f(mut)
          mut.x +=1
          if (test(mut)) f(mut)
          mut.y +=1
          if (test(mut)) f(mut)
          mut.y +=1
          if (test(mut)) f(mut)
          mut.x -=1
          if (test(mut)) f(mut)
          mut.x -=1
          if (test(mut)) f(mut)
          mut.y -=1
          if (test(mut)) f(mut)
        }

        override def foreach[U](f: (Point) => U) = {
          if (simple)
            unchecked(f)
          else {
            checked(f)
          }
        }
      }

      if (mutablePoints) t else t.map(_.copy)
    }

    private val bombCounts = Array.tabulate(width, height)((_,_) => UNKNOWN_STATE)
    private val remainingSurroundingBombs = Array.tabulate(width, height)((_,_) => 0)
    private val bombDetected = Array.tabulate(width, height)((_,_) => false)
   // private val
  }
}

object Utils {

  implicit class PointOps(val p:Point) extends AnyVal {
    def copy = p.clone().asInstanceOf[Point]
  }

  implicit class ArrayArrayOps[T](val a:Array[Array[T]]) extends AnyVal {
    def at(p:Point) = a(p.x)(p.y)
  }

}


