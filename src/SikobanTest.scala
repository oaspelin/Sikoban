
import java.awt.Point

import java.io.File

/**
 * Informaatioverkostojen studio 1 -kurssin viidennen Java-tehtävän testiluokka.
 * Luokka testaa Sikoban-pelin logiikkaa toteuttamalla yksinkertaisen
 * testipohjaisen käyttöliittymån.
 *
 * @author Janne Käki
 * @author Ville Sundberg
 * @author Petteri Noponen
 * @author Anastasia Lipiäinen (Scalannos)
 *
 */
object SikobanTest {
  
  private var game : Game = null
  
  def main(args: Array[String]) {
    try {
      game = new Game(new File("/home/oskar/Eclipse Workspacec/harjoitus_5/src/kentta1.txt"))
      
      while (!game.hasGameFinished()) {
        printGrid()
        val coord : Point = askCoordinates()
        
        if (!game.move(coord.x, coord.y)) {
          println("No, no, no! Not there.")
        } 
      }
      println("You won!")
      println(game.player_location)
    } catch {
      case e : Exception =>
        e.printStackTrace()
    }
  }
  
  private def printGrid() {
    println // one empty line for fun!
    var playerX : Int = -1
    var playerY : Int = -1
    
    for (y <- 0 until game.height) {
      for (x <- 0 until game.width) {
        var tile = game.grid(x)(y)
        
        if (tile == GridType.WALL) {
          System.out.print('#')
        } else if (tile == GridType.OBSTACLE) {
          System.out.print('$')
        } else if (tile == GridType.PLAYER) {
          System.out.print('@')
          playerX = x
          playerY = y
        } else if (tile == GridType.GOAL) {
          System.out.print('G')
        } else {
          System.out.print(' ')
        }
      }
      System.out.println()
    }
    System.out.println("Your position is (" + playerX + "," + playerY+ ").")  
  }
  
  private def askCoordinates() : Point  = {
    var x : Int = -1
    var y : Int = -1
    var OK : Boolean = false
    
    do {
      try {
        System.out.print("Give new X coordinate: ")
        x = Integer.parseInt(readLine())
        if (x >= 0 && x < game.width) {
          OK = true
        }  
      } catch {
        case nfe : NumberFormatException => //nothing
      }
    } while (!OK)
      
    OK = false
    
    do {
      try {
        System.out.print("Give new Y coordinate: ")
        y = Integer.parseInt(readLine())
        if (y >= 0 && y < game.height) {
          OK = true
        }
      } catch {
        case nfe: NumberFormatException => //nothing
      }
    } while (!OK)
      
    new Point(x, y)  
  }

}