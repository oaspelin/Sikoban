import javax.imageio.ImageIO

import java.io.File

import java.awt._


import swing._
import scala.swing.event._
import GridType._
import sokoban._

import scala.swing.event.KeyPressed




class GamePanel(width:Int, height:Int, game:Game) extends GridPanel(width,height)
{
  preferredSize= new Dimension(300,300)
  
  var start=ImageIO.read(new File("src/menu.png")) //file containing the image when game is started
  var win= ImageIO.read(new File("src/win.png")) //image which is displayed if the pllayer has won
  var play=false //keeps track if GamePanel is shown for the first time in order to show start image
  
  override def paintComponent(g: Graphics2D) 
  {	  
	  if(!play)
	  { //draws start image if the game has been started
	    g.drawImage(start,0,0, null)
	    play=true //play is true if game has been started, in order not to show start image anymore
	  }
	  else if(game.hasGameFinished())
	    g.drawImage(win,0,0,null)//draws the "win image" if the player has won
	    
	  else
	  {
		  for(y<-0 until game.height)
		  {
			  for(x<- 0 until game.width)
			  {   
				  g.setColor(getColor(game.grid(x)(y)))
				  g.fillRect(x*30, y*30, 30, 30)
				  addSpecialGraphic(g, x, y) //adds smoother graphics to some tiles
			  }
		  }
	  } 
  }
  
  listenTo(mouse.clicks,keys)
  reactions+=
  {
    
    case a: MouseClicked=>
    	if(!game.hasGameFinished())
    	{
    	  game.move(toCoordinate(a.point.x, true), toCoordinate(a.point.y,false))
    	  sokoban.lab.text= game.getGameStatus() //updates the label text
    	  this.repaint()
    	}   
   
    case b: KeyPressed => //Added keybindings to the game
    {
    	if(!game.hasGameFinished())
    	{
    		if(b.key==Key.Down)
    			game.move(game.player_location._1+getTile("South")._1,game.player_location._2+getTile("South")._2)
    		
    		if(b.key==Key.Up)
    			game.move(game.player_location._1+getTile("North")._1,game.player_location._2+getTile("North")._2)
    	
    	    if(b.key==Key.Left)
    	    	game.move(game.player_location._1+getTile("West")._1,game.player_location._2+getTile("West")._2)
    	    
    	    if(b.key==Key.Right)
    	    	game.move(game.player_location._1+getTile("East")._1,game.player_location._2+getTile("East")._2)
    	}
    	sokoban.lab.text= game.getGameStatus()
    	this.repaint()
    }
    
  }
  focusable=true //in order to get keybindings to work this was needed dunno why
  requestFocus // same as above
      
  
  //function needed for keybindings to work
  def getTile(s:String): (Int,Int)=
  {
    var dir:(Int,Int) = s match 
    {
      case "North" => add("North")
      case "South" => add("South")
      case "West" =>  add("West")
      case "East" =>  add("East")
    }
    return dir
  }
  //needed to impement this to get keybindings to work
  def add(direction: String): (Int,Int)=
  {
     var add= direction match{
      case "North" => Pair(0,-1)
      case "South" => Pair(0,1)
      case "West" => Pair(-1,0)
      case "East" => Pair(1,0)
    }
    return add
  }
  //function that calculates to what tile the player is trying to reach, used by mouseclicks
  def toCoordinate(x:Int, dir:Boolean): Int=
  {
	  var ret=0
	  var len=0
	  var count=1
    
	  if(dir)
		  len=game.width
	  else
		  len=game.height
    
	  for(i<-0 until len)
	  {
		  if((i*30<x) && (x<count*30))
		  {
			  ret=i
		  }
		  count+=1
	  }
	  return ret
 }
  //Just adding some smoother graphics
  def addSpecialGraphic(g:Graphics2D, x:Int, y: Int)
  {
    if(game.grid(x)(y)==EMPTY || game.grid(x)(y)==OBSTACLE) 
    {
    	g.setColor(Color.BLACK)
	    g.drawRect(x*30, y*30, 30, 30)
    }
    if(game.grid(x)(y)==OBSTACLE) 
    {
    	g.setColor(Color.BLACK)
    	g.drawLine(x*30,y*30, (x+1)*30, (y+1)*30)
    	g.drawLine(x*30,(y+1)*30, (x+1)*30, y*30)
    }
    if(game.grid(x)(y)==GOAL)
    { 
    	var odd=true;
    	g.setColor(Color.BLACK)
    	for(i<-0 until 5)
    		for(j<-0 until 5)
    		{
    		    if(odd)
    		    {
    		    	g.fillRect(x*30+i*6,y*30+j*6,6,6)
    		    	odd=false
    		    }
    		    else
    		    {
    		    	odd=true
    		    }
    		 }
    }
    if(game.grid(x)(y)==PLAYER)
    {
    	var img= ImageIO.read(new File("src/possu.gif"))
    	g.drawImage(img, x*30,y*30, null)
    	g.setColor(Color.BLACK)
	    g.drawRect(x*30, y*30, 30, 30)
    }
  }
}//end of class