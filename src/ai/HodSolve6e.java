/* Copyright Hamster Development 2006 */
package ai;

import Sweeper.MineField;
import Sweeper.MineFieldFlag;
import Sweeper.MineFinder;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * Ultimativer Minesweeper-Löser.
 */
public class HodSolve6e
  implements MineFinder
{
// ------------------------------ FIELDS ------------------------------

  private static final AmbigousAreaException AMBIGOUS_AREA_EXCEPTION = new AmbigousAreaException();
  //~ Static fields/initializers
  private static final boolean DEBUG_DETAILS = false;
  private static final int BOMB_COUNT_UNKNOWN_YET = -1;
  private static final int BOMB_FOUND = -2;
  private static final int EVENT_HORIZON = 3;
  private static final String ID_NAME = "HamsterofDeath Minefinder 0.6e";

  //~ Instance fields

  private boolean bombDataModified;

  /**
   * Flag, ob ein Feld als potenzieller Nachbar einer Bombe eingestuft ist
   */
  private boolean[][] isDangerous;
  private boolean[][] isMarkedAsTested;
  private boolean[][] isMarkedAsTestedBackup;
  private boolean[][] isNotSmoothed;
  /**
   * Flag, ob ein Feld vollständig von aufgedeckten Feldern umkreist ist
   */
  private boolean[][] isPrisoner;

  private int bruteForceStepCounter = 0;
  private int dangerousPointCount = 0;
  private int height;
  private int iterations;
  private int remainingBombs;
  private int remainingFields;
  private int stackOverflowErrors = 0;

  private int width;
  private int[][] bcBackup;

  /**
   * Cache für die Werte aufgedeckter Felder, aus Performancegründen
   */
  private int[][] bombCount;

  /**
   * Hilfsvariable ähnlich bombCount, enthält an x/y den Bombcount des Feldes, falls aufgedeckt, abzüglich der Anzahl
   * der in nächster Nähe aufgedeckten Bomben. Standardwert ist 0. Wird eine Bombe gefunden, ist der Wert <= -10. Ist
   * der Wert > 0, sind noch unentdeckte Bomben in der Nähe. Bei 0 kann nur in Kombination mit Bombcount eine sichere
   * Aussage gemacht werden, ob das Feld sicher, oder nur noch nicht aufgedeckt ist.
   */
  private int[][] bombCountSmoothed;
  private MineField mineField;


  private Point[] dangerousPoints;
  private final Point[] eightPointsForCheckUnknownNeighbors = new Point[8];
  private final Point[] eightPointsForGetNeighbors = new Point[8];
  private final Point[] eightPointsForIsBorderPoint = new Point[8];
  private final Point[] eightPointsForIsValid = new Point[8];
  private final Point[] eightPointsForIsValid2 = new Point[8];
  private final Point[] eightPointsForMaybeBombed = new Point[8];
  private final Point[] eightPointsForMaybeFree = new Point[8];
  private final Point[] eightPointsForMaybeFree2 = new Point[8];
  private final Point[] eightPointsForSmoothBombField = new Point[8];

  /**
   * Hilfsvariable aus Performancegründen.
   */
  private final Point[] eightPointsForTraverseNon0Field = new Point[8];
  /**
   * Statischer Speicher für alle potenziell benötigten Punkte, aus Performancegründen
   */
  private Point[][] pointSource;

// --------------------------- CONSTRUCTORS ---------------------------

  //~ Constructors
  /**
   * Creates a new HodSolve5d object.
   */
  public HodSolve6e()
  {
    super();
  }

// ------------------------ INTERFACE METHODS ------------------------

// --------------------- Interface MineFinder ---------------------

  //~ Methods
  /**
   * DOCUMENT ME!
   *
   * @param field DOCUMENT ME!
   */
  public void start(MineField field)
  {
    initSolver(field);

    Point startingPoint = mineField.getStartingPoint();

    try
    {
      solveIt(startingPoint);
    }
    catch (NoMoreBombsException e)
    {
      if (DEBUG_DETAILS)
      {
        System.out.println("*****All bombs found!*****");
      }
    }
    if (DEBUG_DETAILS)
    {
      System.out.printf("%d Stackoverflowerrors occured. \n",
                        stackOverflowErrors);
      int closed = getRemainingFieldCount();
      if (remainingBombs == closed)
      {
        System.out.printf("%d fields & bombs remaining\n", remainingBombs);
      }
      else
      {
        System.out.printf("%d fields & %d bombs remaining\n", closed,
                          remainingBombs);
      }
    }
  } // end method start

  //~

  /**
   * DOCUMENT ME!
   */
  public void stop()
  {
  }

  //~

  /**
   * DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public String getIdentification()
  {
    return ID_NAME;
  }

// -------------------------- OTHER METHODS --------------------------

  /**
   * @param solveEverything
   * @param outline         Alle Punkte einer zusammenhängenden Fläche
   * @param bombs           Hier landen alle Bomben drin
   * @param secureFields    Hier landen alle nicht-Bomben drin
   */
  private void evaluateFieldStates(List<Point> mustBeSmoothed,
                                   List<Point> outline,
                                   Point[] bombs,
                                   int bombCount,
                                   Point[] secureFields,
                                   int secureFieldCount,
                                   Solution foundSolutions,
                                   int index,
                                   final int maxDepth,
                                   int currentDepth,
                                   int bombsLeft, boolean solveEverything) throws AmbigousAreaException
  {
    if (DEBUG_DETAILS)
    {
      bruteForceStepCounter++;
    }
    if (currentDepth++ >= maxDepth || bombsLeft == 0)
    {
      if (solveEverything)
      {
        if (bombsLeft > 0)
        {
          return;
        }
      }
      //Ende eines Astes erreicht. Hier kommen alle Varianten raus, die keine direkten Widerspüche erzeugen.
      //Dabei können allerdings immernoch Kombinationen auftreten, die den geglätteten Bombenzähler nicht auf 0
      //reduzieren, und damit als Lösung nicht in Frage kommen. Diese müssen rausgefiltert werden.
      for (Point field : mustBeSmoothed)
      {
        if (bombCountSmoothed[field.x][field.y] != 0)
        {
          return;
        }
      }
      //Alles klar, gültige Lösung :)
      foundSolutions.merge(new SolvedLine(bombs, bombCount, secureFields, secureFieldCount));
      if (foundSolutions.areAllFieldsAmbigous())
      {
        //Die bisher gefundenen Lösungen widersprechen sich in jedem einzelnen Feld mindestens 1 mal
        //Weitermachen ist sinnlos
        throw AMBIGOUS_AREA_EXCEPTION;
        //return;
      }
      return;
    }
    Point checkMe = outline.get(index);
    int x = checkMe.x;
    int y = checkMe.y;
    isMarkedAsTested[x][y] = true;


    if (maybeBombed(x, y))
    {
      smoothBombfield(x, y);
      bombs[bombCount++] = checkMe;
      evaluateFieldStates(mustBeSmoothed,
                          outline,
                          bombs,
                          bombCount,
                          secureFields,
                          secureFieldCount,
                          foundSolutions,
                          index + 1,
                          maxDepth,
                          currentDepth,
                          bombsLeft - 1, solveEverything);
      deSmoothBombfield(x, y);
      bombCount--;
    }
    if (maybeFree(x, y))
    {
      secureFields[secureFieldCount++] = checkMe;
      evaluateFieldStates(mustBeSmoothed,
                          outline,
                          bombs,
                          bombCount,
                          secureFields,
                          secureFieldCount,
                          foundSolutions,
                          index + 1,
                          maxDepth,
                          currentDepth,
                          bombsLeft, solveEverything);
      secureFieldCount--;
    }
    isMarkedAsTested[x][y] = false;

    //Sackgasse erreicht.
  }

  /**
   * Prüft, ob das Feld an x/y frei sein kann.
   *
   * @param x
   * @param y
   *
   * @return
   */
  private boolean maybeFree(int x, int y)
  {
    Point p;
    Point pInner;
    int xx;
    int yy;
    int freeFieldsAroundXY = 0;
    for (int i = setSurroundingPoints(x, y, eightPointsForMaybeFree, true);
         --i >= 0;)
    {
      //Alle Nachbarn finden, die aufgedeckt sind und einen SBC > 0 haben
      p = eightPointsForMaybeFree[i];
      xx = p.x;
      yy = p.y;

      if (bombCount[xx][yy] > 0 && bombCountSmoothed[xx][yy] > 0)
      {
        //Prüfen, ob diese Felder genug andere freie (aktuell durchgetestete Felder nicht enthalten) Nachbarn haben, die den Bombcount auf 0 setzen könnten.
        for (int j = setSurroundingPoints(xx, yy, eightPointsForMaybeFree2, false); --j >= 0;)
        {
          pInner = eightPointsForMaybeFree2[j];
          if (!isMarkedAsTested[pInner.x][pInner.y])
          {
            //Freies Feld gefunden, das noch nicht durchrekursiviert wurde.
            freeFieldsAroundXY++;
          }
        }
        return freeFieldsAroundXY >= bombCountSmoothed[xx][yy];
      }
    }
    //Bombenfreiheit konnte nicht ausgeschlossen werden
    return true;
  }

  /**
   * Prüft anhand der Werte der das aktuelle Feld umgebenden Felder, ob das Feld eine Bombe enthalten kann, ohne dadurch
   * bereits bekannten Werten zu widersprechen.
   *
   * @param x
   * @param y
   *
   * @return
   */
  private boolean maybeBombed(int x, int y)
  {
    Point p;
    int xx;
    int[] smoothedInts;
    int[] bc;
    int yy;
    for (int added = setSurroundingPoints(x, y, eightPointsForMaybeBombed,
                                          true); --added >= 0;)
    {
      p = eightPointsForMaybeBombed[added];
      xx = p.x;
      smoothedInts = bombCountSmoothed[xx];
      bc = bombCount[xx];
      yy = p.y;
      if (smoothedInts[yy] > 0)
      {
        //Hier könnte eine Bombe liegen, da nicht alle Bombennachbarn von p gefunden worden sind
        return true;
      }
      else if (bc[yy] == 0 || bc[yy] != BOMB_COUNT_UNKNOWN_YET &&
        smoothedInts[yy] == 0)
      {
        //Hier kann unmöglich eine Bombe liegen
        return false;
      }
    }
    //Es konnte nicht ausgeschlossen werden, dass hier eine Bombe liegt
    return true;
  }

  /**
   * Fügt einer Liste Punkte hinzu, die ein bestimmtes Feld einschließen
   *
   * @param x             Umkreister Punkt, X-koordinate
   * @param y             Umkreister Punkt, Y-koordinate
   * @param putHere       Liste, an die Elemente angefügt werden sollen
   * @param includeOpened True, wenn bereits aufgedeckte Felder mit in die Liste gesteckt werden sollen
   *
   * @return Anzahl hinzugefügter Elemente
   */
  private int setSurroundingPoints
    (
      int x,
      int y,
      Point[] putHere,
      boolean includeOpened
    )
  {
    int index = 0;
    int extendedSurrounding = 2;
    int dummy1 = x + extendedSurrounding;
    int maxX = (dummy1 <= width) ? dummy1 : width;
    int dummy2 = y + extendedSurrounding;
    int maxY = (dummy2 <= height) ? dummy2 : height;
    int dummy3 = y - 1;
    int minY = (dummy3 >= 0) ? dummy3 : 0;
    int dummy4 = x - 1;
    int minX = (dummy4 >= 0) ? dummy4 : 0;
    if (includeOpened)
    {
      // "Spezial"-fall: Man braucht genau die 8 umliegenden
      int minXPlus1 = minX + 1;
      int minYPlus1 = minY + 1;
      int minXPlus2 = minX + 2;
      int minYPlus2 = minY + 2;
      if (((minXPlus2 + 1) == maxX) && ((minYPlus2 + 1) == maxY))
      {
        putHere[0] = this.pointSource[minX][minY];
        putHere[1] = this.pointSource[minXPlus1][minY];
        putHere[2] = this.pointSource[minXPlus2][minY];
        putHere[3] = this.pointSource[minX][minYPlus1];
        putHere[4] = this.pointSource[minXPlus2][minYPlus1];
        putHere[5] = this.pointSource[minX][minYPlus2];
        putHere[6] = this.pointSource[minXPlus1][minYPlus2];
        putHere[7] = this.pointSource[minXPlus2][minYPlus2];

        return 8;
      }
      else
      {
        // Beschnittenes Feld:
        int yy;
        for (int xx = minX; xx < maxX; xx++)
        {
          for (yy = minY; yy < maxY; yy++)
          {
            if ((x != xx) || (y != yy))
            {
              putHere[index++] = this.pointSource[xx][yy];
            }
          }
        }
      }
    }
    else
    {
      // "Spezial"-fall: Man braucht genau die 8 umliegenden
      int minXPlus1 = minX + 1;
      int minYPlus1 = minY + 1;
      int minXPlus2 = minX + 2;
      int minYPlus2 = minY + 2;
      if (((minXPlus2 + 1) == maxX) && ((minYPlus2 + 1) == maxY))
      {
        if (BOMB_COUNT_UNKNOWN_YET == bombCount[minX][minY])
        {
          putHere[index++] = this.pointSource[minX][minY];
        }
        if (BOMB_COUNT_UNKNOWN_YET == bombCount[minXPlus1][minY])
        {
          putHere[index++] = this.pointSource[minXPlus1][minY];
        }
        if (BOMB_COUNT_UNKNOWN_YET == bombCount[minXPlus2][minY])
        {
          putHere[index++] = this.pointSource[minXPlus2][minY];
        }
        if (BOMB_COUNT_UNKNOWN_YET == bombCount[minX][minYPlus1])
        {
          putHere[index++] = this.pointSource[minX][minYPlus1];
        }
        if (BOMB_COUNT_UNKNOWN_YET == bombCount[minXPlus2][minYPlus1])
        {
          putHere[index++] = this.pointSource[minXPlus2][minYPlus1];
        }
        if (BOMB_COUNT_UNKNOWN_YET == bombCount[minX][minYPlus2])
        {
          putHere[index++] = this.pointSource[minX][minYPlus2];
        }
        if (BOMB_COUNT_UNKNOWN_YET == bombCount[minXPlus1][minYPlus2])
        {
          putHere[index++] = this.pointSource[minXPlus1][minYPlus2];
        }
        if (BOMB_COUNT_UNKNOWN_YET == bombCount[minXPlus2][minYPlus2])
        {
          putHere[index++] = this.pointSource[minXPlus2][minYPlus2];
        }
      }
      else
      {
        int yy;
        for (int xx = minX; xx < maxX; xx++)
        {
          for (yy = minY; yy < maxY; yy++)
          {
            if ((BOMB_COUNT_UNKNOWN_YET == bombCount[xx][yy]) && ((xx != x)
              || (yy != y)))
            {
              putHere[index++] = this.pointSource[xx][yy];
            }
          }
        }
      }
    } // end if

    return index;
  } // end method setSurroundingPoints

  //~

  private void smoothBombfield(
    int x,
    int y
  )
  {
    Point p;
    for (int i = setSurroundingPoints(x, y, eightPointsForSmoothBombField,
                                      true); --i >= 0;)
    {
      p = eightPointsForSmoothBombField[i];
      bombCountSmoothed[p.x][p.y]--;
    }
  }

  //~

  private void deSmoothBombfield(
    int x,
    int y
  )
  {
    Point p;
    for (int i = setSurroundingPoints(x, y, eightPointsForSmoothBombField,
                                      true); --i >= 0;)
    {
      p = eightPointsForSmoothBombField[i];
      bombCountSmoothed[p.x][p.y]++;
    }
  }

  //~

  private int getRemainingFieldCount()
  {
    int ret = 0;
    int[] ints;
    for (int i = bombCount.length - 1; i >= 0; i--)
    {
      ints = bombCount[i];
      for (int j = ints.length; --j >= 0;)
      {
        if (ints[j] == BOMB_COUNT_UNKNOWN_YET)
        {
          ret++;
        }
      }
    }

    return ret;
  }

  //~

  private void initSolver(MineField field)
  {
    mineField = field;
    remainingBombs = mineField.getBombCount();
    height = mineField.getFieldSize().height;
    width = mineField.getFieldSize().width;
    bombCount = new int[width][height];
    bcBackup = new int[width][height];
    isDangerous = new boolean[width][height];
    isMarkedAsTested = new boolean[width][height];
    isMarkedAsTestedBackup = new boolean[width][height];
    isPrisoner = new boolean[width][height];
    isNotSmoothed = new boolean[width][height];
    remainingFields = width * height;
    for (boolean[] booleans : isNotSmoothed)
    {
      Arrays.fill(booleans, true);
    }
    bombCountSmoothed = new int[width][height];
    for (int[] ints : bombCount)
    {
      Arrays.fill(ints, BOMB_COUNT_UNKNOWN_YET);
    }
    pointSource = new Point[width][height];
    dangerousPoints = new Point[width * height];

    for (int x = 0; x < pointSource.length; x++)
    {
      Point[] points = pointSource[x];
      for (int y = 0; y < points.length; y++)
      {
        points[y] = new Point(x, y);
        isDangerous[x][y] = true;
      }
    }
  } // end method initSolver

  //~

  private void openTraverseField(
    int x,
    int y
  )
    throws NoMoreBombsException
  {
    // Schnell Durchrekursivieren
    Point pointTmp = pointSource[x][y];
    mineField.setFlag(pointTmp, MineFieldFlag.OPEN);
    remainingFields--;

    int bombcount = mineField.getValue(pointTmp);
    bombDataModified = true;
    if (DEBUG_DETAILS)
    {
      System.out.printf("Opening: (%d/%d), Bombcount is %d\n", x + 1, y + 1,
                        bombcount);
    }
    bombCount[x][y] = bombcount;
    traverseField(bombcount, x, y, pointTmp);
  } // end method openTraverseField

  private void traverseField(int bombcount, int x, int y, Point pointTmp)
    throws NoMoreBombsException
  {
    if (bombcount == 0)
    {
      // Sehr schön, alle Felder im Umkreis des aktuellen sind sicher.
      isDangerous[x][y] = false;

      Point[] stupid = new Point[8];
      if (DEBUG_DETAILS)
      {
        System.out.printf("Can open %d fields surrounding (%d/%d)\n",
                          setSurroundingPoints(x, y, stupid, false),
                          x + 1,
                          y + 1);
      }

      Point ep;
      for (int i = setSurroundingPoints(x, y, stupid, false); --i >= 0;)
      {
        ep = stupid[i];
        if (bombCount[ep.x][ep.y] == BOMB_COUNT_UNKNOWN_YET)
        {
          openTraverseField(ep.x, ep.y);
        }
      }
      isPrisoner[x][y] = true;
    }
    else
    {
      if (isNotSmoothed[x][y])
      {
        isNotSmoothed[x][y] = false;
        bombCountSmoothed[x][y] += bombcount;
      }
      dangerousPoints[dangerousPointCount++] = pointTmp;

      // ist, da 0 der standardwert ist
      traverseNon0Field(x, y, bombcount);
    }
  }

  //~

  /**
   * Löst das Spielfeld von einem aufgedeckten Feld aus, dass einen Bombcount > 0 hat
   *
   * @param x
   * @param y
   */
  private void traverseNon0Field(
    int x,
    int y,
    int bombcount
  )
    throws NoMoreBombsException
  {
    int added = setSurroundingPoints(x, y, eightPointsForTraverseNon0Field,
                                     true);
    int potentialBombs = 0;

    // Umliegende Felder nach bereits aufgedeckten Bomben durchsuchen und den Wert des zu prüfenden
    // Feldes runterzählen
    Point p;
    int bc;
    int i;
    for (i = added; --i >= 0;)
    {
      p = eightPointsForTraverseNon0Field[i];
      bc = bombCount[p.x][p.y];
      if (bc == BOMB_FOUND)
      {
        bombcount--;
      }
      else if (bc == BOMB_COUNT_UNKNOWN_YET)
      {
        potentialBombs++;
      }
    }
    if ((bombcount > 0) && (bombcount == potentialBombs))
    {
      // Alle umliegenden Felder müssen Bomben sein
      for (i = added; --i >= 0;)
      {
        p = eightPointsForTraverseNon0Field[i];
        if (BOMB_COUNT_UNKNOWN_YET == bombCount[p.x][p.y])
        {
          revealBomb(p.x, p.y);
        }
      }
      if (DEBUG_DETAILS)
      {
        System.out.printf(
          "Field (%d/%d) %d surrounded by %d (not opened) bomb(s) and %d opened fields found, %d bomb(s) left\n",
          x,
          y,
          bombcount,
          bombcount,
          8 - bombcount,
          remainingBombs
        );
      }
      isDangerous[x][y] = false;
      isPrisoner[x][y] = true;
    }
    else if ((bombcount == 0) && (potentialBombs > 0))
    {
      // Alle Bomben wurden schon entdeckt, in diesem Fall können alle das aktuelle Feld umgebenden
      // verdeckten Felder gefahrlos aufgedeckt werden
      if (DEBUG_DETAILS)
      {
        System.out.printf(
          "Can open %d fields surrounding (%d/%d), all near bombs already found\n",
          potentialBombs,
          x,
          y
        );
      }

      Point[] stupid = new Point[added];
      System.arraycopy(eightPointsForTraverseNon0Field, 0, stupid, 0,
                       added);
      for (i = added; --i >= 0;)
      {
        p = stupid[i];
        if (BOMB_COUNT_UNKNOWN_YET == bombCount[p.x][p.y])
        {
          openTraverseField(p.x, p.y);
        }
      }
      isDangerous[x][y] = false;
      isPrisoner[x][y] = true;
    }
    else if (potentialBombs == 0)
    {
      // Alle umliegenden Felder wurden bereits aufgedeckt
      isDangerous[x][y] = false;
      isPrisoner[x][y] = true;
    }
  } // end method traverseNon0Field

  //~

  private void revealBomb(
    int x,
    int y
  )
    throws NoMoreBombsException
  {
    bombCount[x][y] = BOMB_FOUND;
    bombCountSmoothed[x][y] -= 10;
    mineField.setFlag(pointSource[x][y], MineFieldFlag.FLAGGED);
    bombDataModified = true;
    remainingBombs--;
    if (DEBUG_DETAILS)
    {
      System.out.printf("Found Bomb at (%d/%d) , %d bomb(s) left\n", x + 1,
                        y + 1, remainingBombs);
    }
    if (remainingBombs == 0)
    {
      throw new NoMoreBombsException();
    }
    smoothBombfield(x, y);
  }

  //~

  private void solveIt(Point startingPoint)
    throws NoMoreBombsException
  {
    boolean tryAgain = false;
    do
    {
      tryAgain = false;
      if (startingPoint == null)
      {
        rescanComplete();
      }
      else
      {
        //Schnell und schluderig übers Feld flitzen
        traverseSecure(startingPoint);
        // Nächstes sicheres Feld bestimmen
        startingPoint = getNext();
      }

      // Wenn nichts mehr geht, geht vielleicht Brute force
      if (startingPoint == null)
      {
        if (DEBUG_DETAILS)
        {
          System.out.println("\n*** Switching to bruteforce method ***\n");
        }
        Solution solution = evaluateSolutions();
        tryAgain = !solution.isEmpty();
        SolvedLine solvedLine = solution.getMergedSolution();
        {
          for (Point bomb : solvedLine.bombs)
          {
            //Bomben aufdecken
            revealBomb(bomb.x, bomb.y);
          }
          for (Point secureField : solvedLine.secureFields)
          {
            //Schnell und schluderig übers Feld flitzen
            traverseSecure(secureField);
          }
        }
        if (DEBUG_DETAILS)
        {
          System.out.println("\n*** Switching back to default method ***\n");
        }
      }
    }
    while (startingPoint != null || tryAgain);
  } // end method solveIt

  /**
   * Kompletter Feldscan
   *
   * @throws ai.NoMoreBombsException
   */
  private void rescanComplete()
    throws NoMoreBombsException
  {
    do
    {
      bombDataModified = false;

      int[]     ys;
      int bombCount;
      int y;
      Point[] points;
      boolean[] prisoner;
      for (int x = this.bombCount.length; --x >= 0;)
      {
        // isDangerous = this.isDangerous[x];
        prisoner = isPrisoner[x];
        ys = this.bombCount[x];
        points = pointSource[x];
        for (y = height; --y >= 0;)
        {
          if (!prisoner[y] && ((bombCount = ys[y]) >= 0) && isBorderPoint(x,
                                                                          y))
          {
            try
            {
              traverseField(bombCount, x, y, points[y]);
            }
            catch (StackOverflowError e)
            {
              //Kackmist :(
              stackOverflowErrors++;
            }
          }
        }
      }
    }
    while (bombDataModified);
  } // end method rescanField

  /**
   * @param x
   * @param y
   *
   * @return True, wenn der Punkt Nachbar von mind. einem aufgedeckten Feld ist
   */
  private boolean isBorderPoint(int x, int y)
  {
    Point p;
    for (int i = setSurroundingPoints(x, y, eightPointsForIsBorderPoint,
                                      true); --i >= 0;)
    {
      p = eightPointsForIsBorderPoint[i];
      if (bombCount[p.x][p.y] != BOMB_COUNT_UNKNOWN_YET)
      {
        return true;
      }
    }
    return false;
  }

  //~

  /**
   * Erbruteforced sich ein freies Feld, von wo aus weiter mit dem Standard-algorithmus gelöst werden kann
   */
  private Point getNext()
  {
    int[] row;
    int bc;
    int y;

//     Das Feld nach einem Startpunkt durchsuchen, von diesem aus seine Nachbarfelder finden, die
//     an aufgedeckten Feldern angrenzen
    for (int x = width; --x >= 0;)
    {
      row = bombCount[x];
      for (y = height; --y >= 0;)
      {
        bc = row[y];
        if ((bc == BOMB_COUNT_UNKNOWN_YET) && checkIfFieldIsSecure(x, y,
                                                                   EVENT_HORIZON))
        {
          return pointSource[x][y];
        }
      }
    }

    // Ende Gelände :(
    return null;
  } // end method getNext

  //~

  private boolean checkIfFieldIsSecure(
    int x,
    int y,
    int range
  )
  {
    // Diesen Punkt prüfen
    int leftX = x - range;
    int upperY = y - range;
    int rightX = 1 + x + range;
    int lowerY = 1 + y + range;

    int fromY = (0 >= upperY) ? 0 : upperY;
    int fromX = (0 >= leftX) ? 0 : leftX;
    int toX = (width <= rightX) ? width : rightX;
    int toY = (height <= lowerY) ? height : lowerY;

    return checkIfRectangleIsSecure(fromX, toX, fromY, toY, x, y);
  }

  //~

  /**
   * Prüft, ob sich an x/y in dem Rechteck von fromX/fromY nach toX/toY eine Bombe befinden kann
   *
   * @param fromX inklusiv
   * @param toX   exklusiv
   * @param fromY inklusiv
   * @param toY   exklusiv
   * @param x
   * @param y
   *
   * @return Den passenden Punkt
   */
  private boolean checkIfRectangleIsSecure(
    int fromX,
    int toX,
    int fromY,
    int toY,
    int x,
    int y
  )
  {
    smoothBombfield(x, y);
    if (DEBUG_DETAILS)
    {
      System.out.printf(
        "Rectangle from (%d/%d) to (%d/%d) area check around point (%d/%d) for guaranteed security\n",
        fromX + 1,
        fromY + 1,
        toX,
        toY,
        x + 1,
        y + 1
      );
    }

    int yy;
    int bcs;
    int[] column;
    int[] column2;
    for (int xx = fromX; xx < toX; xx++)
    {
      column = bombCountSmoothed[xx];
      column2 = bombCount[xx];
      for (yy = fromY; yy < toY; yy++)
      {
        bcs = column[yy];
        if ((bcs > 0) && (column2[yy] >= 0))
        {
          if (isImpossibleSituation(xx, yy, bcs))
          {
            if (DEBUG_DETAILS)
            {
              System.out.printf(
                "Invalid value (smoothed bombcount is %d) found at (%d/%d), depth %d, (%d/%d) cannot be a bomb\n",
                bombCountSmoothed[xx][yy],
                xx + 1,
                yy + 1,
                Math.max(Math.abs(xx - x), Math.abs(yy - y)),
                x + 1,
                y + 1
              );
            }
            deSmoothBombfield(x, y);

            return true;
          }
        }
      }
    } // end for
    deSmoothBombfield(x, y);

    return false;
  } // end method checkIfRectangleIsSecure

  //~

  /**
   * @param x
   * @param y
   * @param bombcount
   *
   * @return True, wenn dieser Bombcount an der übergebenen Position sein kann. False, wenn das mit Sicherheit
   *         ausgeschlossen werden kann.
   */
  private boolean isImpossibleSituation(
    int x,
    int y,
    int bombcount
  )
  {
    Point p;
    int free = 0;
    boolean isFree;
    int j;
    Point pp;
    for (int i = setSurroundingPoints(x, y, eightPointsForIsValid, true);
         --i >= 0;)
    {
      p = eightPointsForIsValid[i];

      // Freie umgebende Felder finden, in denen Bomben sein können
      if (bombCount[p.x][p.y] == BOMB_COUNT_UNKNOWN_YET)
      {
        // Felder ausschließen, die Nachbarn von 0-feldern im Smooth-feld sind
        isFree = true;
        for (j = setSurroundingPoints(p.x, p.y, eightPointsForIsValid2,
                                      true); --j >= 0;)
        {
          pp = eightPointsForIsValid2[j];
          if (bombCount[pp.x][pp.y] >= 0)
          {
            isFree &= bombCountSmoothed[pp.x][pp.y] > 0;
          }
        }
        if (isFree)
        {
          if (++free >= bombcount)
          {
            return false;
          }
        }
      }
    }

    return true;
  } // end method isImpossibleSituation

  /**
   * Versucht, den Rest des Feldes per Bruteforce zu knacken
   */
  private Solution evaluateSolutions()
  {
    List<List<Point>> areas = new ArrayList<List<Point>>();
    Solution ret = new Solution();
    //Noch offene Flächen finden, dann alle Möglichkeiten (Bombe/frei) durchtesten, falls nötig Schnittmenge bilden.
    boolean[][] traversed = new boolean[width][height];
    LinkedList<Point> bombs = new LinkedList<Point>();
    LinkedList<Point> secureFields = new LinkedList<Point>();
    for (int x = width; --x >= 0;)
    {
      for (int y = height; --y >= 0;)
      {
        if (!traversed[x][y] && bombCount[x][y] == BOMB_COUNT_UNKNOWN_YET)
        {
          //Startpunkt für die aktuelle Fläche festlegen
          List<Point> area = findOutline(x, y, traversed, new
            ArrayList<Point>());
          areas.add(area);
        }
      }
    }
    //Kleinere Flächen zuerst scannen
    Collections.sort(areas, new Comparator<List<Point>>()
    {
      public int compare(List<Point> o1, List<Point> o2)
      {
        return o1.size() - o2.size();
      }
    });
    //Versuchen, jede Fläche einzeln für sich zu lösen
    boolean solutionFoundPerArea = false;
    for (List<Point> area : areas)
    {
      SolvedLine result = null;
      try
      {
        result = evaluateFieldStates(area, false);
        //Erfolgreiche Lösungen sammeln
        if (result != null && (!result.bombs.isEmpty() ||
          !result.secureFields.isEmpty()))
        {
          solutionFoundPerArea = true;
          ret.add(result);
        }
      }
      catch (OutOfMemoryError e)
      {
        //Och nö...
        if (DEBUG_DETAILS)
        {
          System.out.println("**** Out of memory! :(, rebuilding smoothed bc data ****");
        }
        //Die BC-Smoothed-daten könnten zerstört worden sein, neu aufbauen
        rebuildSmoothedBCData();
      }
      catch (StackOverflowError e)
      {
        //Och nö...
        if (DEBUG_DETAILS)
        {
          System.out.println("**** Stack overflow error! :(, rebuilding smoothed bc data ****");
        }
        //Die BC-Smoothed-daten könnten zerstört worden sein, neu aufbauen
        rebuildSmoothedBCData();
      }
    }
    //Das letze As aus dem Ärmel ziehen, falls der Check per Flächen nichts ergeben hat
    if (!solutionFoundPerArea && (areas.size() > 1 ||
      areas.get(0).size() < (remainingFields - (mineField.getBombCount() - remainingBombs))))
    {
      if (DEBUG_DETAILS)
      {
        System.out.printf("Bruteforce per area failed, using bruteforce on whole field\n");
      }
      ret.add(evaluateFieldStates(getWholeField(), true));
    }

    return ret;
  }

  /**
   * Rekursiviert vom angegebenen Punkt aus durch, wie groß das Feld der nicht aufgedeckten Felder ist
   *
   * @param traversed
   *
   * @return
   */
  private List<Point> findOutline(int x, int y, boolean[][] traversed,
                                  List<Point> area)
  {
    if (isBorderPoint(x, y))
    {
      area.add(pointSource[x][y]);
    }
    traversed[x][y] = true;
    Point[] ps = new Point[8];
    Point[] psInner = new Point[8];
    Point p;
    Point pInner;
    int xx;
    int yy;
    int bc;
    int j;
    for (int i = setSurroundingPoints(x, y, ps, true); --i >= 0;)
    {
      p = ps[i];
      xx = p.x;
      yy = p.y;
      if (!traversed[xx][yy])
      {
        bc = bombCount[xx][yy];
        if (bc == BOMB_COUNT_UNKNOWN_YET)
        {
          //wir sind innerhalb unserer fläche
          findOutline(xx, yy, traversed, area);
        }
        else
        {
          //Die aktuelle Fläche kann eine benachbarte, die nur durch 1 Feld  getrennt ist, beeinflussen. Dieses muss mit ins aktuelle Feld aufgenommen werden, sonst werden evtl. Möglichkeiten beim Bruteforcen übersehen
          for (j = setSurroundingPoints(xx, yy, psInner, false); --j >= 0;)
          {
            pInner = psInner[j];
            if (!traversed[pInner.x][pInner.y])
            {
              //Und weiter im Nachbarfeld
              findOutline(pInner.x, pInner.y, traversed, area);
            }
          }
        }
      }
    }
    return area;
  }

  /**
   * Smoothed-Bombcount-array neu aufbauen
   */
  private void rebuildSmoothedBCData()
  {
    for (int[] ints : bombCountSmoothed)
    {
      Arrays.fill(ints, 0);
    }
    for (int x = bombCountSmoothed.length; --x >= 0;)
    {
      for (int y = bombCountSmoothed[x].length; --y >= 0;)
      {
        if (bombCount[x][y] == BOMB_FOUND)
        {
          smoothBombfield(x, y);
          bombCountSmoothed[x][y] -= 10;
        }
        else if (bombCount[x][y] > 0)
        {
          bombCountSmoothed[x][y] += bombCount[x][y];
        }
      }
    }
  }

  private SolvedLine evaluateFieldStates(List<Point> outline, boolean solveEverything)
  {
    int maxDepth = outline.size();
    if (DEBUG_DETAILS)
    {
      System.out.printf("Checking " + (solveEverything ? "area" : "line") + " containing point (%d/%d), size %d\n",
                        outline.get(0).x + 1,
                        outline.get(0).y + 1,
                        maxDepth);
    }
    Solution solutions = new Solution();
    bruteForceStepCounter++;
    Point[] dummyBombs = new Point[outline.size()];
    Point[] dummySecureFields = new Point[outline.size()];
    backupBCData();
    try
    {
      evaluateFieldStates(getNeighborsOfOutline(outline),
                          outline,
                          dummyBombs,
                          0,
                          dummySecureFields,
                          0,
                          solutions,
                          0,
                          maxDepth,
                          0,
                          remainingBombs, solveEverything);
    }
    catch (AmbigousAreaException e)
    {
      restoreSmoothedBCData();
    }

    if (DEBUG_DETAILS)
    {
      System.out.printf("Scanned " + (solveEverything ? "area" : "line") +
        " with %d fields containing point (%d/%d), calculated %d steps\n",
                        outline.size(),
                        outline.get(0).x + 1,
                        outline.get(0).y + 1,
                        bruteForceStepCounter);
    }
    //Wenn es mehr als eine mögliche Lösung gibt, die Lösungen auf
//Gemeinsamkeiten abscannen.
    SolvedLine finalSolution = solutions.getMergedSolution();
    if (DEBUG_DETAILS)
    {
      System.out.printf("Mergeresult: %d bombs, %d secure fields\n",
                        finalSolution.bombs.size(),
                        finalSolution.secureFields.size());
    }
    return finalSolution;
  }

  private void backupBCData()
  {
    for (int i = bombCountSmoothed.length; --i >= 0;)
    {
      System.arraycopy(bombCountSmoothed[i], 0, bcBackup[i], 0, width);
    }
    for (int i = isMarkedAsTested.length; --i >= 0;)
    {
      System.arraycopy(isMarkedAsTested[i], 0, isMarkedAsTestedBackup[i], 0, width);
    }
  }

  /**
   * Findet alle aufgedeckten nicht-Bomben-Felder, die unmittelbar an die übergebenen Felder angrenzen
   *
   * @param outline
   *
   * @return
   */
  private List<Point> getNeighborsOfOutline(List<Point> outline)
  {
    List<Point> ret = new ArrayList<Point>(outline.size());//Größe dürfte hinkommen
    Set<Point> filterDoubles = new HashSet<Point>((outline.size() << 2) /
      3);
    for (Point p : outline)
    {
      for (int i = setSurroundingPoints(p.x, p.y,
                                        eightPointsForGetNeighbors, true); --i >= 0;)
      {
        p = eightPointsForGetNeighbors[i];
        if (bombCount[p.x][p.y] >= 0 && checkUnkownNeighbors(p, outline))
        {
          filterDoubles.add(p);
        }
      }
    }
    ret.addAll(filterDoubles);
    return ret;
  }

  /**
   * @param checkMyNeighbors
   * @param outline
   *
   * @return True, wenn alle nicht verdeckten Felder um p herum in outline liegen
   */
  private boolean checkUnkownNeighbors(Point checkMyNeighbors, List<Point>
    outline)
  {
    Point p;
    for (int i = setSurroundingPoints(checkMyNeighbors.x,
                                      checkMyNeighbors.y,
                                      eightPointsForCheckUnknownNeighbors,
                                      false); --i >= 0;)
    {
      p = eightPointsForCheckUnknownNeighbors[i];
      if (!outline.contains(p))
      {
        return false;
      }
    }
    return true;
  }

  private void restoreSmoothedBCData()
  {
    for (int i = bombCountSmoothed.length; --i >= 0;)
    {
      System.arraycopy(bcBackup[i], 0, bombCountSmoothed[i], 0, width);
    }
    for (int i = isMarkedAsTested.length; --i >= 0;)
    {
      System.arraycopy(isMarkedAsTestedBackup[i], 0, isMarkedAsTested[i], 0, width);
    }

  }

  private List<Point> getWholeField()
  {
    List<Point> ret = new ArrayList<Point>(remainingFields);
    for (int doubleRow = 0; doubleRow + 1 < height; doubleRow += 2)
    {
      for (int x = width; --x >= 0;)
      {
        if (bombCount[x][doubleRow]==BOMB_COUNT_UNKNOWN_YET)
        {
          ret.add(pointSource[x][doubleRow]);
        }
        if (bombCount[x][doubleRow+1]==BOMB_COUNT_UNKNOWN_YET)
        {
          ret.add(pointSource[x][doubleRow+1]);
        }
      }
    }
    if (height%2!=0)
    {
      for (int x = width; --x >= 0;)
      {
        if (bombCount[x][height-1]==BOMB_COUNT_UNKNOWN_YET)
        {
          ret.add(pointSource[x][height-1]);
        }
      }
    }
    return ret;
  }

  private void traverseSecure(Point startingPoint)
    throws NoMoreBombsException
  {
    try
    {
      if (DEBUG_DETAILS)
      {
        System.out.printf("\n\nIteration %d -----------------------------\n\n", ++iterations);
      }

      // Triviale aufdecken
      if (bombCount[startingPoint.x][startingPoint.y] ==
        BOMB_COUNT_UNKNOWN_YET)
      {
        openTraverseField(startingPoint.x, startingPoint.y);
        // Alle auf dem Weg als unsicher markierten Felder nochmal scannen,vielleicht hat sich ja was neues ergeben.
        rescanDangerous();
      }
    }
    catch (StackOverflowError e)
    {
      //Super, ein Stackoverflow-error.
      stackOverflowErrors++;
      rescanComplete();
    }
  }

  //~

  private void rescanDangerous()
    throws NoMoreBombsException
  {
    do
    {
      bombDataModified = false;

      int x;
      int y;
      Point df;
      for (int i = 0; i < dangerousPointCount; i++)
      {
        df = dangerousPoints[i];
        x = df.x;
        y = df.y;
        if (isDangerous[x][y])
        {
          //Feld scannen
          traverseNon0Field(x, y, this.bombCount[x][y]);
        }
        else
        {
          //Felder rauswerfen, die nicht mehr gescannt werden müssen
          dangerousPoints[i] = dangerousPoints[--dangerousPointCount];
        }
      }
    }
    while (bombDataModified);
  } // end method rescanField

// -------------------------- INNER CLASSES --------------------------

  /**
   * Container für eine mögliche Lösung einer Reihe von verdeckten Felder am Rand von Aufgedeckten.
   */
  private static class SolvedLine
  {
// ------------------------------ FIELDS ------------------------------

    List<Point> bombs;
    List<Point> secureFields;

// --------------------------- CONSTRUCTORS ---------------------------

    /**
     * Speichert die übergebenen Punkte in eigenen Listen. Die übergebenen Parameter können weiterverwendet werden
     *
     * @param bombs
     * @param secureFields
     */
    public SolvedLine(Collection<Point> bombs, Collection<Point>
      secureFields)
    {
      this.bombs = new ArrayList<Point>(bombs);
      this.secureFields = new ArrayList<Point>(secureFields);
    }

    public SolvedLine(Point[] bombs, int bombCount, Point[] secureFields, int secureFieldCount)
    {
      this.bombs = new ArrayList<Point>(bombCount);
      for (int i = 0; i < bombCount; i++)
      {
        this.bombs.add(bombs[i]);
      }
      this.secureFields = new ArrayList<Point>(secureFieldCount);
      for (int i = 0; i < secureFieldCount; i++)
      {
        this.secureFields.add(secureFields[i]);
      }
    }
  }

  private static class Solution
  {
// ------------------------------ FIELDS ------------------------------

    private boolean isAmbigous;
    private List<Point> bombs;
    private List<Point> secureFields;

// -------------------------- OTHER METHODS --------------------------

    public void add(SolvedLine o)
    {
      if (bombs == null)
      {
        bombs = new ArrayList<Point>(o.bombs);
      }
      else
      {
        bombs.addAll(o.bombs);
      }
      if (secureFields == null)
      {
        secureFields = new ArrayList<Point>(o.secureFields);
      }
      else
      {
        secureFields.addAll(o.secureFields);
      }
      isAmbigous = bombs != null && bombs.isEmpty() && secureFields != null && secureFields.isEmpty();
    }

    public boolean areAllFieldsAmbigous()
    {
      return isAmbigous;
    }

    /**
     * Erzeugt aus allen enthaltenen SolvedLines eine einzige, die die gemeinsamen Felder und Bomben enthält
     */
    public SolvedLine getMergedSolution()
    {
      return new SolvedLine(bombs == null ? new ArrayList<Point>(0) : bombs,
                            secureFields == null ? new
                              ArrayList<Point>(0) : secureFields);
    }

    public boolean isEmpty()
    {
      return (bombs == null || bombs.isEmpty()) &&
        (secureFields == null || secureFields.isEmpty());
    }

    public void merge(SolvedLine o)
    {
      if (bombs == null)
      {
        bombs = new ArrayList<Point>(o.bombs);
      }
      else if (bombs.size() > 0)
      {
        bombs.retainAll(o.bombs);
      }
      if (secureFields == null)
      {
        secureFields = new ArrayList<Point>(o.secureFields);
      }
      else if (secureFields.size() > 0)
      {
        secureFields.retainAll(o.secureFields);
      }
      isAmbigous = bombs != null && bombs.isEmpty() && secureFields != null && secureFields.isEmpty();
    }
  }

  private static class NoMoreBombsException extends Exception
  {
  }

  private static class AmbigousAreaException extends Exception
  {
  }
} // end class HodSolve6d
