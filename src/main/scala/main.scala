/* ------------------------------------------------------------
   !Scala+Processing 3D 切断テスト
------------------------------------------------------------ */
import processing.core._

//
case class Vector(x:Float, y:Float, z:Float)
{
}

// 
object Color
{
  def Red = Color(255, 0, 0)
  def Green = Color(0, 255, 0)
  def Blue = Color(0, 0, 255)
}

// 
case class Color(r:Float, g:Float, b:Float)
{
}

// 
case class Vertex(color:Color, pos:Vector)
{
}

/* ------------------------------------------------------------
 !アプレット
 !@memo
 ------------------------------------------------------------ */
object CutTestApplet extends PApplet
{
  // 
  var game: Game = new Game()

  // 
  def main(args: Array[String])
  {
    var frame = new javax.swing.JFrame("Scala Cut")
    var applet = CutTestApplet

    frame.getContentPane().add(applet)
    applet.init()
    frame.pack()
    frame.setVisible(true)
  }

  // 
  override def setup()
  {
    game.setup(this)
  }

  // 
  override def draw()
  {
    game.draw()

    if(game.isQuit())
    {
      noLoop()
      System.exit(0)
    }
  }

  // 
  override def mouseReleased()
  {
    println("mouseReleased: " + mouseX + " " + mouseY + "button " + mouseButton)

    game.mouseReleased(mouseX, mouseY, mouseButton)
  }
  // 
  override def mousePressed()
  {
    println("mousePressed: " + mouseX + " " + mouseY + "button " + mouseButton)

    game.mousePressed(mouseX, mouseY, mouseButton)
  }

  // 
  override def keyPressed()
  {
    println("keyPressed: " + key + ", State " + game.getState())

    game.keyPressed(key)
  }
}
/* ------------------------------------------------------------
 !ゲーム管理
 !@memo
------------------------------------------------------------ */
class Game()
{
  private val uiFontSize = 14
  private var numFont: PFont = new PFont
  private var uiFont: PFont = new PFont
 
  private val GameStop = 0
  private val GamePlay = 1
  private val GameClear = 2
  private val GameOver = 3
  private val GameQuit = 4
  private var state = GameStop
  private var startTime = 0
  private var lastTime = 0
  private var app:PApplet = null

  def isQuit(): Boolean = state == GameQuit
  def getState() = state

  val width = 640
  val height = 640

  // 
  def setup(g: PApplet)
  {
    app = g
    app.size(width, height, PConstants.P3D)

    numFont = app.createFont("SanSerif", uiFontSize)
    uiFont = app.createFont("SanSerif", uiFontSize)

    app.frameRate(60)
    app.textFont(numFont)
    app.background(255)

    reset()
  }

  //
  def reset()
  {
    state = GameStop
  }

  // 
  def mouseReleased(mouseX:Int, mouseY:Int, mouseButton: Int)
  {
    if(state == GameStop || state == GamePlay)
    {
      if(mouseButton == PConstants.LEFT)
      {
      }
    }
  }

  // 
  def mousePressed(mouseX:Int, mouseY:Int, mouseButton: Int)
  {
    if(state == GameStop || state == GamePlay)
    {
      if(mouseButton == PConstants.LEFT)
      {
      }
    }
  }

  //
  def exit()
  {
    println("exit.")

    state = GameQuit
  }

  // mouseボタンが押しづらいのでキーボードでも押せるようにする
  def keyPressed(key:Int)
  {
    if(state == GameStop || state == GamePlay)
    {
      // key match
      // {
	    //   case 'a' => open(g.mouseX, g.mouseY)
	    //   case 'f' => frag(g.mouseX, g.mouseY)
	    //   case _ => ()
      // }
    }
    if(state != GameQuit)
    {
      key match
      {
	      case 'r' => reset()
	      case 'q' => exit()
	      case _ => ()
      }
    }
  }
  
  // 
  def draw()
  {
    app.background(255)

    // draw Header
    app.textFont(uiFont)
    app.stroke(0)
    app.fill(0)

    val time = (app.millis() / 1000 ).toString
    app.text(" time: " + time + "sec", 4, 4 + uiFontSize)

    // 3D rendering
    app.background(188)

    app.pushMatrix()     
    app.translate(width/2, height/2, -100)
        
    app.scale(90)
    app.rotateY(PConstants.PI/6)
    app.beginShape(PConstants.QUADS)

    val vertex = List(
      Vertex(Color(255, 255, 255), Vector(1, 1, 1)),
      Vertex(Color(0, 255, 255), Vector(-1, 1, 1)),
      Vertex(Color(255, 0, 255), Vector(1, -1, 1)),
      Vertex(Color(255, 255, 0), Vector(1, 1, -1)),
      Vertex(Color(255, 0, 0), Vector(1, -1, -1)),
      Vertex(Color(0, 255, 0), Vector(-1, 1, -1)),
      Vertex(Color(0, 0, 255), Vector(-1, -1, 1)),
      Vertex(Color(0, 0, 0), Vector(-1, -1, -1))
    )
    val index = List(
      1, 0, 2, 6,
      // 0, 3, 4, 2,
      // 3, 5, 7, 4,
      // 5, 1, 6, 7,
      // 5, 3, 0, 1,
      // 7, 4, 2, 6
    )

    for(i <- index)
    {
      val v = vertex(i)
      app.fill(v.color.r, v.color.g, v.color.b)
      app.vertex(v.pos.x, v.pos.y, v.pos.z)
    }
        
    app.endShape()
    
    app.popMatrix() 
  }
}
