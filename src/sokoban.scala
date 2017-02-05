import swing._
import javax.swing.KeyStroke
import javax.imageio.ImageIO
import java.io.File





import scala.swing.event._
object sokoban extends SimpleSwingApplication
{
	private var game : Game = null
	private var _lab= new Label()
	def lab = _lab
	private var grids :GamePanel= null
	
	def top= new MainFrame()
    {
	 	title="Sokoban"
	    preferredSize= new Dimension(310,375) //game fits fully inside this size
	 	iconImage=ImageIO.read(new File("src/possu.gif"))
	 	game = new Game(new File("src/kentta1.txt")) //I didn't find it necessary to try=>catch here since it is taken care of in the initGame function
	 	grids= new GamePanel(game.width,game.height, game)
	 	lab.text="Click to start game" //label text at the start of the game
	 	

	 	var bar=new MenuBar()
	 	{
	 		contents+= new Menu("Game")
	 		{
	 			contents+= new MenuItem(new Action("Start again")
	 			{
	 			  def apply()
	 			  {
	 				  game.startOver(new File("src/kentta1.txt"))
	 				  grids=new GamePanel(game.width,game.height, game)
	 				  lab.text="Game has been restarted" //changes the label text since game has been started again
	 			  }
	 			  accelerator=Some(KeyStroke.getKeyStroke("F2"))
	 			})
	 			
	 			contents+= new MenuItem(new Action("Quit")
	 			{
	 			  def apply()
	 			  {
	 				  quit()
	 			  }
	 			  accelerator=Some(KeyStroke.getKeyStroke("alt F4"))
	 			})
	 		}
	 	}
	 
	 	contents= new BorderPanel
	 	{	
	 		preferredSize= new Dimension(300,300)
	 		add(grids, BorderPanel.Position.Center)
	 		add(bar, BorderPanel.Position.North)
	 		add(lab, BorderPanel.Position.South)
	 	}
	 	

     centerOnScreen()
   } 
}

	
  
 


