import java.io.File
import java.util._
import GridType._

object test{
  
	var grids=Array.ofDim[GridType.Value](0,0)
	def main(args: Array[String]){
	val filelocation="/home/oskar/Eclipse Workspacec/harjoitus_5/src/kentta1.txt"
	var scan=new Scanner(new File(filelocation))
	
	val width= scan.nextInt()
	scan.nextLine
	val height= scan.nextInt()

	scan.useDelimiter("")
	grids=Array.ofDim[GridType.Value](width,height);
	var player_location=Pair(0,0)
	for(j<- 0 until height)
	{
	  scan.nextLine
	  for(i<-0 until width)
	  {
	    grids(i)(j)=translateToGridType(scan.next())
	    var player_location=Pair(i,j)
	  }
	}
	
	println(player_location)
	println(canMoveTo(grids, player_location,0,1))
	
	println(getGridType(3,5))
	}
	
	def canMoveTo(grids:Array[Array[GridType.Value]], player_location:(Int,Int),  x:Int, y:Int) : Boolean = {
    var ret=false
    
    var direction= getDirection(player_location, x, y)
    var add= direction match{
      case "North" => Pair(0,1)
      case "South" => Pair(0,-1)
      case "West" => Pair(-1,0)
      case "East" => Pair(1,0)
    }
    
    if(!Valid(player_location, x,y,grids) && (grids(x)(y)==GridType.EMPTY) || ((grids(x)(y)==GridType.OBSTACLE)&& (grids(x+add._1)(y+add._2)==GridType.EMPTY)))
    	ret=true
    	  
    return ret
  }
	def Valid(player_location:(Int,Int), x:Int, y:Int, grids:Array[Array[GridType.Value]]): Boolean=
  {
    var ret=true  
    if( ((player_location._1-x)^2 + (player_location._2-x)^2) != 1)
      ret=false
    
    if( (0<x && x< grids.length) && (0<y && y<grids(0).length) )
      ret=true
    
    return ret
  }
	  private def getDirection(player_location:(Int,Int), x:Int, y:Int): String =
  {
    var direction= "South"
	var horizontal=true
	
	if(player_location._2 !=y) 
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
	
	def getGridType(x:Int, y:Int) : GridType.Value = {
    return grids(x)(y)
	}
	
	def translateToGridType (s:String) : GridType.Value = {
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
}



