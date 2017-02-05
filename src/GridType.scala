import java.awt.Color

object GridType extends Enumeration
{
	type grid=Value
	val PLAYER, WALL, OBSTACLE, GOAL, EMPTY= Value
    
	def getColor(colour: grid): Color=
	{
	  var col:Color= colour match 
	  {
	    case PLAYER => Color.WHITE
	    case WALL => new Color(21,19,19)
	    case OBSTACLE => new Color(255,165,0)
	    case GOAL => Color.WHITE
	    case EMPTY => Color.WHITE
	  }
	  return col
	}
}
	