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

  class Stats {
    var bombs                    = 0
    var safeTotal                = 0
    var putInSafeList            = 0
    var duplicatedInSafeList     = 0
    var putInUnsafeList          = 0
    var duplicateInUnsafeList    = 0
    var putInExpensiveList       = 0
    var duplicateInExpensiveList = 0
    var gotStuck                 = 0
    var resortedToBruteForce     = 0
    var bruteForceSuccess        = 0
    var bruteForceFailure        = 0
    var bruteForcedSolutions     = 0

    override def toString =
      s"""|bombs:                        $bombs,
          |safeTotal:                    $safeTotal,
          |putInSafeList:                $putInSafeList,
          |duplicatedInSafeList:         $duplicatedInSafeList,
          |putInUnsafeList:              $putInUnsafeList,
          |duplicateInUnsafeList:        $duplicateInUnsafeList,
          |putInExpensiveList:           $putInExpensiveList,
          |duplicateInExpensiveList:     $duplicateInExpensiveList,
          |gotStuck:                     $gotStuck,
          |resortedToBruteForce:         $resortedToBruteForce,
          |bruteForceFailure:            $bruteForceFailure,
          |bruteForceSuccess:            $bruteForceSuccess,
          |bruteForcedSolutions:         $bruteForcedSolutions
          |""".stripMargin
  }

  def solve(myField: FieldData, nativeField: MineField): Unit = {
    val stats = new Stats

    var safeTodo = mutable.ArrayBuffer(myField.start)
    var unsafeTodo = mutable.ArrayBuffer.empty[Point]
    var expensiveTodo = mutable.ArrayBuffer.empty[Point]


    def flagAsBomb(p: Point): Unit = {
      nativeField.setFlag(p, MineFieldFlag.FLAGGED)
      myField.informBomb(p)
      stats.bombs += 1
    }

    def quickSolve() = {
      var change = false
      while (run && (safeTodo.nonEmpty || unsafeTodo.nonEmpty || expensiveTodo.nonEmpty)) {

        def flagAsBombInternally(p: Point): Unit = {
          flagAsBomb(p)
          change = true
        }

        def openField(p: Point): Unit = {
          nativeField.setFlag(p, MineFieldFlag.OPEN)
          val bombCount = nativeField.getValue(p)
          myField.informSafe(p, bombCount)
          change = true
          stats.safeTotal += 1
          if (bombCount == 0) {
            safeTodo ++= myField.around(p, withHidden = true, recycleInstance = false)
          } else {
            unsafeTodo ++= myField.around(p, withHidden = true, recycleInstance = false)
          }
        }

        def handleSafe(): Unit = {
          val next = safeTodo.remove(safeTodo.size - 1)
          stats.putInSafeList += 1
          if (myField.isClosed(next)) {
            openField(next)
          } else {
            stats.duplicatedInSafeList += 1
          }
        }

        def handleUnsafe(): Unit = {
          val next = unsafeTodo.remove(unsafeTodo.size - 1)
          stats.putInUnsafeList += 1
          if (myField.isClosed(next)) {
            if (myField.mustBeSafe(next)) {
              openField(next)
            } else if (myField.mustBeBomb(next)) {
              flagAsBombInternally(next)
              unsafeTodo ++= myField.around(next, withHidden = true, recycleInstance = false)
            } else {
              expensiveTodo += next
            }
          } else {
            stats.duplicateInUnsafeList += 1
          }
        }

        def handleExpensive(): Unit = {
          val next = expensiveTodo.remove(expensiveTodo.size - 1)
          stats.putInExpensiveList += 1
          if (myField.isClosed(next)) {
            if (myField.mustBeSafeViaContradiction(next)) {
              openField(next)
            }
          } else {
            stats.duplicateInExpensiveList += 1
          }

          if (expensiveTodo.isEmpty && change) {
            change = false
            val nextTries = myField.closedOutline.filter { p =>
              myField.nextToOpen(p)
            }
            stats.gotStuck += 1
            unsafeTodo ++= nextTries
          }
        }

        if (safeTodo.nonEmpty) {
          handleSafe()
        } else if (unsafeTodo.nonEmpty) {
          handleUnsafe()
        } else if (expensiveTodo.nonEmpty) {
          handleExpensive()
        }
      }
      change
    }

    def deepSolve() = {
      stats.resortedToBruteForce += 1
      def traverseArea(p: Point): Traversable[Point] = {
        new Traversable[Point] {

          private val memberOfOutline = myField.closedOutline.toSet

          override def foreach[U](f: (Point) => U) = {
            val processed = mutable.HashSet.empty[Point]
            val open = mutable.LinkedHashSet.empty[Point]
            open += p
            while (open.nonEmpty) {
              val next = open.head
              f(next)
              processed += next
              open.remove(next)
              myField.around(next, withHidden = true, prioritizeNear = true)
              .foreach { candidate =>
                if (memberOfOutline(candidate) && !processed(candidate)) {
                  open += candidate.copy
                }
              }
            }
          }
        }
      }

      val areas = {
        val separated = mutable.ArrayBuffer.empty[collection.Set[Point]]
        myField.closedOutline.foreach { start =>
          val skip = separated.exists(_ (start))
          if (!skip) {
            separated += traverseArea(start).to[mutable.LinkedHashSet]
          }
        }
        separated.sortBy(_.size)
      }

      case class Solution(bombs: List[Point], safe: List[Point]) {
        def useful = bombs.nonEmpty || safeTodo.nonEmpty
      }

      val maxLimit = 40
      def findSolutionForGivenLineOfPoints(connectedPointsInArea: Traversable[Point]) = {
        val full = connectedPointsInArea.toList
        val alwaysBombs = mutable.HashSet.empty[Point] ++= full
        val alwaysSafe = mutable.HashSet.empty[Point] ++= full
        val maxBombs = myField.bombsLeft
        val maxFree = myField.freeLeft

        def simulate(current: Point, remaining: List[Point], remainingDepth: Int, bombs: mutable.Set[Point],
                     safe: mutable.Set[Point], bombsToUse: Int, freeToUse: Int): Unit = {
          val goDeeper = remainingDepth > 0 && (alwaysBombs.nonEmpty || alwaysSafe.nonEmpty) && remaining.nonEmpty

          def storeConclusion() = {
            alwaysBombs.retain(bombs)
            alwaysSafe.retain(safe)
            stats.bruteForcedSolutions += 1
          }

          val nextDepth = remainingDepth - 1
          if (bombsToUse > 0 && myField.canBeBomb(current)) {
            myField.pretendBomb(current)
            bombs += current
            if (goDeeper) {
              simulate(remaining.head, remaining.tail, nextDepth, bombs, safe, bombsToUse - 1, freeToUse)
            } else {
              storeConclusion()
            }
            bombs -= current
            myField.unPretendBomb(current)
          }
          if (freeToUse > 0 && myField.canBeSafe(current)) {
            safe += current
            myField.pretendSafe(current)
            if (goDeeper) {
              simulate(remaining.head, remaining.tail, nextDepth, bombs, safe, bombsToUse, freeToUse - 1)
            } else {
              storeConclusion()
            }
            myField.unpretendSafe(current)
            safe -= current
          }
        }

        simulate(full.head, full.tail, maxLimit, mutable.Set.empty, mutable.Set.empty, maxBombs, maxFree)

        Solution(alwaysBombs.toList, alwaysSafe.toList)
      }

      val solutions = {
        // i am sure there is a way to do this better, but too much research is required
        val partial = areas
        val all = mutable.ArrayBuffer(areas.flatten.to[mutable.LinkedHashSet])
        val preparedAreasToCheck = {
          val chunked = (areas ++ all).flatMap(_.sliding(20, 15)) ++
                        (areas ++ all).flatMap(_.sliding(maxLimit, maxLimit / 2))
          (chunked ++ areas).distinct.sortBy(_.size)
        }
        preparedAreasToCheck.iterator.map(findSolutionForGivenLineOfPoints).filter(_.useful)
      }

      if (solutions.isEmpty) {
        stats.bruteForceFailure += 1
        false
      } else {
        val applyMe = solutions.next()
        stats.bruteForceSuccess += 1
        safeTodo ++= applyMe.safe
        applyMe.bombs.foreach(flagAsBomb)
        applyMe.bombs.foreach { bomb =>
          unsafeTodo ++= myField.around(bomb, withHidden = true, recycleInstance = false)
        }
        true
      }
    }

    var changed = false
    do {
      changed = false
      if (!myField.solved) {
        changed |= quickSolve()
        if (!changed && !myField.solved) {
          changed |= deepSolve()
        }
      }
    }
    while (changed)

    println(stats)
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

  case class FieldData(width: Int, height: Int, bombs: Int, start: Point) {
    def solved = freeFieldsLeft == 0 || remainingBombs == 0

    private val size           = width * height
    private var freeFieldsLeft = size - bombs
    def bombsLeft = remainingBombs
    def freeLeft = freeFieldsLeft

    def nextToOpen(p: Point) = {
      around(p, withOpen = true).nonEmpty
    }

    def closedOutline: Traversable[Point] = new Traversable[Point] {
      override def foreach[U](f: (Point) => U) = {
        val p = new Point
        for (x <- 0 until width; y <- 0 until height) {
          p.move(x, y)
          if (isClosed(p) && around(p, withOpen = true, recycleInstance = false).nonEmpty) {
            f(p.copy)
          }
        }
      }
    }

    def isClosed(where: Point): Boolean = !isOpen(where) &&
                                          !bombDetected.at(where)

    def isOpen(where: Point) = bombCounts.at(where) != UNKNOWN_STATE

    def mustBeSafe(where: Point) = {
      around(where, withOpen = true).exists { box =>
        remainingSurroundingBombs.at(box) == 0
      }
    }

    def canBeBomb(where: Point) = !mustBeSafe(where)
    def canBeSafe(where: Point) = !mustBeBomb(where)

    def pretendBomb(where: Point) = {
      informBomb(where)
    }

    def rectangleAround(where: Point, range: Int): Traversable[Point] = {
      val minX = 0 max (where.x - range)
      val maxX = width min (where.x + range + 1)
      val minY = 0 max (where.y - range)
      val maxY = height min (where.y + range + 1)

      new Traversable[Point] {
        override def foreach[U](f: (Point) => U) = {
          val p = new Point
          for (x <- minX until maxX; y <- minY until maxY) {
            p.move(x, y)
            f(p)
          }
        }
      }
    }

    def mustBeSafeViaContradiction(where: Point) = {
      def contradiction(p: Point) = {
        val reference = remainingSurroundingBombs.at(p)
        def possible = around(p, withHidden = true).count { canIBeABomb =>
          !around(canIBeABomb, withOpen = true).exists { checkIfIAmZero =>
            remainingSurroundingBombs.at(checkIfIAmZero) == 0
          }
        }
        reference > 0 && reference > possible
      }

      pretendBomb(where)
      val safe = rectangleAround(where, 2).exists(contradiction)
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
          if (bomb) "B" else if (isClosed(new Point(x, y))) "?" else count
        }).mkString
      }).mkString("\n")
    }

    def mustBeBomb(where: Point) = {
      pretendOpen(where)
      val bomb = around(where, withOpen = true).exists { box =>
        val possibleRemainingBombs = around(box, withHidden = true).size
        possibleRemainingBombs < remainingSurroundingBombs.at(box)
      }
      unpretendOpen(where)
      bomb
    }

    def pretendSafe(where: Point): Unit = {
      pretendOpen(where)
      remainingSurroundingBombs(where.x)(where.y) -= 100
    }

    def pretendOpen(where: Point): Unit = {
      bombCounts(where.x)(where.y) = 0
    }

    def unpretendOpen(where: Point): Unit = {
      bombCounts(where.x)(where.y) = UNKNOWN_STATE
    }

    def unpretendSafe(where: Point): Unit = {
      unpretendOpen(where)
      remainingSurroundingBombs(where.x)(where.y) += 100
    }

    def allAround(where: Point) = around(where, withHidden = true, withOpen = true)

    def informBomb(where: Point): Unit = {
      remainingBombs -= 1
      unknown -= 1
      allAround(where).foreach { where =>
        remainingSurroundingBombs(where.x)(where.y) -= 1
      }
      bombDetected(where.x)(where.y) = true
    }

    def unPretendBomb(where: Point): Unit = {
      remainingBombs += 1
      allAround(where).foreach { where =>
        remainingSurroundingBombs(where.x)(where.y) += 1
      }
      bombDetected(where.x)(where.y) = false
    }

    private var unknown        = width * height
    private var remainingBombs = bombs

    def informSafe(where: Point, bombCount: Int): Unit = {
      bombCounts(where.x)(where.y) = bombCount
      remainingSurroundingBombs(where.x)(where.y) += bombCount

      unknown -= 1
      freeFieldsLeft -= 1
    }

    def around(p: Point, withHidden: Boolean = false,
               withOpen: Boolean = false,
               recycleInstance: Boolean = true,
               prioritizeNear: Boolean = false): Traversable[Point] = {
      val withAll = withHidden && withOpen
      val base: Traversable[Point] = new Traversable[Point] {

        def isInBoundsUnknown(p: Point) = inBounds(p) && isUnknown(p)

        def inBounds(p: Point) = {
          p.x >= 0 &&
          p.y >= 0 &&
          p.x < width &&
          p.y < height
        }

        def isInBoundsKnown(p: Point) = inBounds(p) && isKnown(p)
        def isUnknown(p: Point) = isClosed(p)
        def isKnown(p: Point) = bombCounts.at(p) != UNKNOWN_STATE
        def simple = notLeft && notRight && notTop && notBottom
        def notLeft = p.x > 0
        def left = !notLeft
        def notRight = p.x < width - 1
        def right = !notRight
        def notTop = p.y > 0
        def top = !notTop
        def notBottom = p.y < height - 1
        def bottom = !notBottom

        def checked[U](f: Point => U) = {
          allWithTest(f, if (withAll) inBounds else if (withHidden) isInBoundsUnknown else isInBoundsKnown)
        }

        def unchecked[U](f: Point => U) = {
          if (withAll) {
            all(f)
          } else {
            allWithTest(f, if (withHidden) isUnknown else isKnown)
          }
        }

        def all[U](f: (Point) => U) = {
          if (prioritizeNear) {
            allLRTBFirst(f)
          } else {
            allClockWise(f)
          }
        }

        def allLRTBFirst[U](f: (Point) => U): Unit = {
          val mut = PointStack.pop(p.x - 1, p.y)
          f(mut) // left
          mut.x += 2
          f(mut) // right
          mut.x -= 1
          mut.y -= 1
          f(mut) // top
          mut.y += 2
          f(mut) // bottom
          mut.x -= 1
          f(mut) // bottom left
          mut.x += 2
          f(mut) // bottom right
          mut.y -= 2
          f(mut) // top right
          mut.x -= 2
          f(mut) // top left
          PointStack.push()
        }

        def allClockWise[U](f: (Point) => U): Unit = {
          val mut = PointStack.pop(p.x - 1, p.y - 1)
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

        def allWithTest[U](f: (Point) => U, test: Point => Boolean) = {
          if (prioritizeNear) {
            allWithTestLRTBFirst(f, test)
          } else {
            allWithTestClockWise(f, test)
          }
        }

        def allWithTestLRTBFirst[U](f: (Point) => U, test: (Point) => Boolean) = {
          val mut = PointStack.pop(p.x - 1, p.y)
          if (test(mut)) f(mut) // left
          mut.x += 2
          if (test(mut)) f(mut) // right
          mut.x -= 1
          mut.y -= 1
          if (test(mut)) f(mut) // top
          mut.y += 2
          if (test(mut)) f(mut) // bottom
          mut.x -= 1
          if (test(mut)) f(mut) // bottom left
          mut.x += 2
          if (test(mut)) f(mut) // bottom right
          mut.y -= 2
          if (test(mut)) f(mut) // top right
          mut.x -= 2
          if (test(mut)) f(mut) // top left
        }

        def allWithTestClockWise[U](f: (Point) => U, test: (Point) => Boolean) = {
          val mut = PointStack.pop(p.x - 1, p.y - 1)
          if (test(mut)) f(mut)
          mut.x += 1
          if (test(mut)) f(mut)
          mut.x += 1
          if (test(mut)) f(mut)
          mut.y += 1
          if (test(mut)) f(mut)
          mut.y += 1
          if (test(mut)) f(mut)
          mut.x -= 1
          if (test(mut)) f(mut)
          mut.x -= 1
          if (test(mut)) f(mut)
          mut.y -= 1
          if (test(mut)) f(mut)
        }

        override def foreach[U](f: (Point) => U) = {
          try {
            if (simple)
              unchecked(f)
            else {
              checked(f)
            }
          } finally {
            PointStack.push()
          }
        }
      }

      if (recycleInstance) base
      else {
        val store = new mutable.ArrayBuffer[Point](8)
        base.foreach { p =>
          store += p.copy
        }
        store
      }
    }

    private val bombCounts                = Array.tabulate(width, height)((_, _) => UNKNOWN_STATE)
    private val remainingSurroundingBombs = Array.tabulate(width, height)((_, _) => 0)
    private val bombDetected              = Array.tabulate(width, height)((_, _) => false)
  }

}

object Utils {

  implicit class PointOps(val p: Point) extends AnyVal {
    def copy = p.clone().asInstanceOf[Point]
  }

  implicit class ArrayArrayOps[T](val a: Array[Array[T]]) extends AnyVal {
    def at(p: Point) = a(p.x)(p.y)
  }

  object PointStack {
    private var given = 0
    private val pool  = mutable.ArrayBuffer.empty[Point]

    def pop(x: Int, y: Int) = {
      if (pool.size <= given) {
        pool += new Point
      }
      val p = pool(given)
      given += 1
      p.move(x, y)
      p
    }

    def push() = {
      given -= 1
    }
  }

}


