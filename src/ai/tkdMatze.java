/* Copyright Hamster Development 2006 */
package ai;

import Sweeper.MineField;
import Sweeper.MineFieldFlag;
import Sweeper.MineFinder;

import java.awt.Point;
import java.util.Vector;

/**
 * Developed with pleasure :)
 *
 * @author HamsterofDeath
 */
public class tkdMatze implements MineFinder
{
final static int VERDECKT =8,BOMBE =9;

    private boolean m_bContinue;
    private MineField Feld;
    int[][] belegt;
    Vector nichtnull = null;
    int bomben = 0;
    int changes = 1;

    public tkdMatze() {
        m_bContinue = true;
    }

    public void start(MineField field) {
        nichtnull = new Vector();
        Feld = field;
        belegt = new int[Feld.getFieldSize().width][Feld.getFieldSize().height];
        for (int i = 0; i < Feld.getFieldSize().height; i++) {
            for (int j = 0; j < Feld.getFieldSize().width; j++) {
                belegt[j][i] = VERDECKT;

            }
        }

        deckeAuf(Feld.getStartingPoint().x, Feld.getStartingPoint().y);
        int ax,ay;
        while ((Feld.getBombCount() != bomben)) {
            changes=1;
            while (changes != 0) {
                changes = 0;
                for (int i = 0; i < nichtnull.size(); i++) {
                  ax=((Point) nichtnull.get(i)).x;
                  ay=((Point) nichtnull.get(i)).y;
                  trivial(ax,ay, i);
                 }
            }
            for (int i = 0; i < nichtnull.size(); i++) {
                  ax=((Point) nichtnull.get(i)).x;
                  ay=((Point) nichtnull.get(i)).y;
                    if (belegt[ax][ay]==2) dreifelder121(ax,ay,i);
                    if (belegt[ax][ay]==1)
                      if (!dreifelder111(ax,ay,i)){

                      }
                }

                for (int i = 0; i < Feld.getFieldSize().height; i++) {
                            for (int j = 0; j < Feld.getFieldSize().width; j++) {
       //  System.out.print(""+belegt[j][i]);

                            }
     //                       System.out.println("");
        }
     //   System.out.println("");

         }

    }





    private void trivial(int x, int y, int z) {
        boolean frei = (belegt[x][y] == 0);
        boolean allestot = (belegt[x][y] == unbekannteDrumHerum(x, y));
        for (int i = x - 1; i < x + 2; i++) {
            for (int j = y - 1; j < y + 2; j++) {
                if (valid(i, j)) {
                    if (frei) {
                        deckeAuf(i, j);
                    } else if (allestot && belegt[i][j] == VERDECKT) {
                        bombeAuf(i, j);
                    }
                }
            }
        }
        if ((frei) || (allestot)) {
            nichtnull.remove(z);
            changes++;
        }
    }


    private void bombeAuf(int x, int y) {
        Feld.setFlag(new Point(x, y), MineFieldFlag.FLAGGED);
        belegt[x][y] = BOMBE;
        bomben++;
        for (int i = x - 1; i < x + 2; i++) {
            for (int j = y - 1; j < y + 2; j++) {
                if (valid(i, j) && (belegt[i][j] != VERDECKT) && (belegt[i][j] != BOMBE)) {
                    belegt[i][j]--;
                    if (belegt[i][j] == 0) {
                        deckeAuf(i, j);
                    }
                }
            }
        }
    }

    private int unbekannteDrumHerum(int x, int y) {
        int ret = 0;
        for (int i = x - 1; i < x + 2; i++) {
            for (int j = y - 1; j < y + 2; j++) {
                if (valid(i, j) && belegt[i][j] == VERDECKT) {
                    ret++;
                }
            }
        }
        return ret;
    }


    private void deckeAuf(int x, int y) {
        if (valid(x, y) && (belegt[x][y] == VERDECKT) &&
            (Feld.getFlag(new Point(x, y)) != MineFieldFlag.FLAGGED)) { //also if sicher
            Feld.setFlag(new Point(x, y), MineFieldFlag.OPEN);
            belegt[x][y] = Feld.getValue(new Point(x, y));
            nichtnull.add(new Point(x,y));
             for (int i = x - 1; i < x + 2; i++) {
                for (int j = y - 1; j < y + 2; j++) {
                    if (valid(i, j)) {
                      if ((belegt[i][j] == BOMBE)) {
                            belegt[x][y]--;
                        }
                    }
                }
            }
          }
    }

  /**
   * dreifelder
   */
  private void dreifelder121(int dx,int dy,int z) {
       if (valid(dx - 1, dy) && belegt[dx - 1][dy] == 1) {
          if (valid(dx + 1, dy) && belegt[dx + 1][dy] == 1) {
            //121 gefunden waagerecht ;)
       //     System.out.println("121");
            if (valid(dx,dy-1)&&belegt[dx][dy-1]==VERDECKT){
              bombeAuf(dx-1,dy-1);
              deckeAuf(dx,dy-1);
              bombeAuf(dx+1,dy-1);

            }else{
              bombeAuf(dx-1,dy+1);
              deckeAuf(dx,dy+1);
              bombeAuf(dx+1,dy+1);

            }
            nichtnull.remove(z);
          }
          if (valid(dx, dy - 1) && belegt[dx][dy - 1] == 1) {
            if (valid(dx, dy + 1) && belegt[dx][dy + 1] == 1) {
                //121 gefunden waagerecht ;)
                   //   System.out.println("121");
                if (valid(dx-1,dy)&&belegt[dx-1][dy]==VERDECKT){
              bombeAuf(dx-1,dy-1);
              deckeAuf(dx-1,dy);
              bombeAuf(dx-1,dy+1);

            }else{
              bombeAuf(dx+1,dy-1);
              deckeAuf(dx+1,dy);
              bombeAuf(dx+1,dy+1);

            }
            nichtnull.remove(z);
            }
          }
        }
   }

   private boolean dreifelder111(int dx,int dy,int z) {
        if (valid(dx - 1, dy) && belegt[dx - 1][dy] == 1) {
           if (valid(dx + 1, dy) && belegt[dx + 1][dy] == 1) {
               if ((!valid(dx -2, dy)) ||(!valid(dx +2, dy)) || belegt[dx - 2][dy] == BOMBE|| belegt[dx + 2][dy] == BOMBE) {
                 //111 gefunden waagerecht ;)
                 //System.out.println("111");
                 if (valid(dx, dy - 1) && belegt[dx][dy - 1] == VERDECKT) {
                   deckeAuf(dx - 1, dy - 1);
                   bombeAuf(dx, dy - 1);
                   deckeAuf(dx + 1, dy - 1);

                 }
                 else {
                   deckeAuf(dx - 1, dy + 1);
                   bombeAuf(dx, dy + 1);
                   deckeAuf(dx + 1, dy + 1);

                 }
                 nichtnull.remove(z);
                 return true;
               }
           }
           if (valid(dx, dy - 1) && belegt[dx][dy - 1] == 1) {
             if (valid(dx, dy + 1) && belegt[dx][dy + 1] == 1) {
                              if ((!valid(dx , dy-2)) ||(!valid(dx , dy+2)) || (belegt[dx ][dy-2] == BOMBE&& belegt[dx ][dy+2] == BOMBE)) {
                                //111 gefunden waagerecht ;)
       //                         System.out.println("111");
                                if (valid(dx - 1, dy) &&
                                    belegt[dx - 1][dy] == VERDECKT) {
                                  deckeAuf(dx - 1, dy - 1);
                                  bombeAuf(dx - 1, dy);
                                  deckeAuf(dx - 1, dy + 1);

                                }
                                else {
                                  deckeAuf(dx + 1, dy - 1);
                                  bombeAuf(dx + 1, dy);
                                  deckeAuf(dx + 1, dy + 1);

                                }
                                nichtnull.remove(z);
                                return true;
                              }
             }
           }
         }
         return false;
    }



  public boolean valid(int x, int y) {
        return (((x) >= 0) && ((y) >= 0) &&
                ((x) < Feld.getFieldSize().width) &&
                ((y) < Feld.getFieldSize().height));
    }

    public void stop() {
        m_bContinue = false;
    }

    public String getIdentification() {
        return "Weltbeherrscherskript";
    }

}
