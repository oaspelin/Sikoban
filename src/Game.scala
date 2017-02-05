import java.io.FileNotFoundException
import java.io.File
import java.util._

import GridType._

import Math._
import java.awt.Point
  



class Game(startLevel:File) extends Object {

  try 
  { // try to open the level file
    initGame(startLevel)
  } 
  catch 
  {   
    case a: IllegalArgumentException => println("Invalid field")
    case b: NumberFormatException => println("Bad input")
    case c:	FileNotFoundException => System.err.println("Couldn't load file")
    case d: NoSuchElementException => println("Width or height is missing")
    case e: Exception => e.printStackTrace()
    case _: Throwable => println("Something went wrong, please check the file for the field")
  }
  

  //some getters and setters
  def player_location= _player_location
  def goal_location = _goal_location
  def grid= _grid
  def original_grid= _original_grid
  def height= _height
  def height_= (value: Int): Unit =height= value
  def width=_width
  def width_= (value: Int): Unit=width= value
  def moves=_moves
  

  
  /**
   * Creates a new game grid by reading it from file specified by <code>f</code>. The file should be
   * a text file (.txt).
   * 
   * Structure of the file is:
   * width of the field as positive integer value
   * height of the field as positive integer value
   * field specified with following characters:
   * "@" - for <code>PLAYER</code
   * "#" - for <code>WALL</code>
   * "$" - for <code>OBSTACLE</code>
   * "G" - for <code>GOAL</code>
   * " " - (space) for <code>EMPTY</code>
   * 
   * If the width or height is not positive <code>IllegalArgumentException</code> will be thrown. If
   * either of width or height is not an integer or one of them is missing
   * <code>NumberFormatException</code> will be thrown.
   * 
   * If the field does not contain a player or contains several goals it will not be accepted (and
   * <code>IllegalArgumentException</code> will be thrown).
   * 
   * If the field specification is wider or higher than specified by given values, extra characters
   * will be disregarded.
   * 
   * If the field contains any other characters they are seen as walls.
   * 
   * @param f	a file that contains game's field specification
   */
  private def initGame(f:File) : Unit = 
  {
    var scan= new Scanner(f)
	
    _width= scan.nextInt()
	_height= scan.nextInt()
	
	if(width+height==width)//error handling
	  throw new NoSuchElementException
	if(width<0 || height<0)//error handling
	  throw  new IllegalArgumentException
	  
	if(!width.isValidInt || !height.isValidInt) //also error handling
	  throw new NumberFormatException
	
	scan.useDelimiter("")//delimiter so that the program is able to scan each character individually
	_grid=Array.ofDim[GridType.Value](width,height)
	
	var player=0//keeps track that there is only one player on the field
	var goal= 0//keeps track that there is only one goal on the field
	for(j<- 0 until height)
	{
		scan.nextLine
		for(i<-0 until width)
		{
			_grid(i)(j)=translateToGridType(scan.next()) //translates the characters to gridtypes
			if(grid(i)(j)==PLAYER)
			{
				player+=1 //atleas one player on field
				_player_location=(i,j)//sets the player location
			}
			if(grid(i)(j)==GOAL)
			{
				goal+=1 //there is atleast one goal on the field
				_goal_location=(i,j) //sets goal location
			}
		}
	}
	
	_original_grid=grid //stores the original grid
	_moves=0 //sets moves to 0, so that they can be tracked

	if( (player!=1) || (goal!=1) ) //error handling, there must be exactly one player and one goal on the field
	  throw new IllegalArgumentException
	
	player=0 //init ends, no need to keep track on theese anymore
	goal=0 //same as above
  }
  



 /**
   * Help method that translates <code>String</code>s to <code>GridType</code>s.  
   * 
   * @param s	the <code>String</code> representation of a tile
   * @param x	the x-coordinate of the tile
   * @param y	the y-coordinate of the tile
   * @return	the <code>GridType</code> value represented by the <code>String</code> <code>s</code>
   */
  private def translateToGridType (s:String) : GridType.Value =  //translates a string to gridtype 
  {  															 // I didn't find it necessary to pass x and y values to this function	
    var t:GridType.Value= s match 
	  {
	    case "@" => PLAYER
	    case "#" => WALL
	    case "$" => OBSTACLE
	    case "G" => GOAL 
	    case " " => EMPTY 
	  }
	  return t
  }
  
  //GetGridType function was not necessary for me, already had a getter
  /**
   * Helping method that tells if the game is ended. Game end when the <code>PLAYER</code reaches
   * the <code>GOAL</code>.
   * 
   * @return	<code>true</code> if the game has finished, otherwise <code>false</code>
   */
  def hasGameFinished() : Boolean = 
  {
    if(player_location==goal_location)
      return true
    else
        return false
  }
  
  
  /**
   * Verifies if moving from players current position to specified position is possible (with one
   * move).
   * 
   * Moving is possible only if the specified tile is <code>EMPTY</code> or if it's an
   * <code>OBSTACLE</code> and the tile behind it is <code>EMPTY</code>.
   * 
   * Moving through <code>WALL</code>s, farther than to the neighboring tile, diagonally or pushing
   * obstacles outside the game area is not permitted. 
   * 
   * @param x	the x-coordinate where player is trying to move
   * @param y	the y-coordinate where player is trying to move
   * @return	<code>true</code> if moving is possible, otherwise <code>false</code>
   */
  private def canMoveTo(x:Int, y:Int) : Boolean = 
  {
	var adds=add(getDirection(x, y))
	var check=false
	
	//these all conditions must be fulfilled for the move to be valid
	if( (Valid(x,y) && (grid(x)(y)==EMPTY || grid(x)(y)==GOAL || (grid(x)(y)==OBSTACLE && isInside(x+adds._1,y+adds._2) && grid(x+adds._1)(y+adds._2)==EMPTY))))
        check=true
    
    if(check)
	  return true
	
	else
	  return false
  }
  
  private def add(direction: String): (Int,Int)= //function that I needed moving blocks to the right place
  {
     var add= direction match{
      case "North" => Pair(0,1)
      case "South" => Pair(0,-1)
      case "West" => Pair(-1,0)
      case "East" => Pair(1,0)
    }
    return add
  }
  //Checks that the target tile is inside the grid
  private def isInside(x:Int, y:Int): Boolean =
  {
   if( (0<=x && x<grid.length) && (0<=y && y< grid(0).length) )
      return true 
   else
      return false
  }
  
  //makes sure that the tile is inside the grid and that player is trying to move only one step
  private def Valid(x:Int, y:Int): Boolean=
  {
    var ret=false  
    if(isInside(x,y) && ( abs(player_location._1-x) + abs(player_location._2-y) == 1 ))
      ret=true
    
    
    return ret
  }
  
  //Checks in which direction the player is trying to move
  private def getDirection(x:Int, y:Int): String =
  {
    var direction= "South"
	var horizontal=true

	if(player_location._2 !=y)  //if player moving horizontally
		horizontal=false
	
	if(horizontal)
	{
		if(player_location._1>x)
			direction="West"
	    else
	    	direction="East"   
	}
    
    if(!horizontal)
	{
    	if(player_location._2>y)
    		direction="South"
	    else
	    	direction="North"   
	}      
	    
	return direction
  }
  
  /**
   * Moves the player in the specified location (if it is possible). The method takes care that
   * the state of the field (including players moves and possible obstacle moves) and players
   * position is updated.
   * 
   * @param x	the x-coordinate where player is trying to move
   * @param y	the y-coordinate where player is trying to move
   * @return	<code>true</code> if move successful, otherwise <code>false</code>
   */
  def move(x:Int, y:Int) : Boolean = 
  {
   
   var adds= add(getDirection(x,y))
   if(canMoveTo(x, y) )
   {
      if(grid(x)(y)==OBSTACLE)
      {
    	  _grid(x+adds._1)(y+adds._2)=OBSTACLE //moves an obstacle to the right tile
      }
      //these are making changes on the ggrid
      _grid(player_location._1)(player_location._2)=EMPTY 
      _player_location=(x,y)
      _grid(x)(y)=PLAYER
      _moves+=1
      
      return true
   }
   else
	   return false
  }
  
  /**
   * Returns the state of the game field to the state that it was in the beginning of the game.
   * The player starts from the initial place and all obstacles are in the original positions.
   */
  def startOver() : Unit = 
  {
    _grid=this.original_grid
  }
  
  /**
   * Another version of <code>startOver</code> method that takes a new game field file as an
   * argument. The game is started from the state that the new file specifies.
   * 
   * @param f	the file containing details of the new field
   */
  def startOver(f:File) : Unit = 
  {
    initGame(f:File)
  }
  
  def getGameStatus(): String=
  {
	if(hasGameFinished)
      return "You Won!"
    else
      return "Game in progress   Moves: "+moves
  }
  
  //variables needed for the implementation
  private var _player_location: (Int,Int)=_
  private var _goal_location: (Int,Int)= _
  private var _grid : Array[Array[GridType.Value]]=_
  private var _original_grid :Array[Array[GridType.Value]]=_
  private var _width: Int=_
  private var _height: Int= _
  private var _moves: Int=_

}//end of class


