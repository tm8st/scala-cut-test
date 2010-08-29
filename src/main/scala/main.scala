/* ------------------------------------------------------------
   !Scala+Processing 3D 切断テスト
------------------------------------------------------------ */
import processing.core._
import tm8st.math._
import tm8st.mesh._
  
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
  private var numFont:PFont = new PFont
  private var uiFont:PFont = new PFont
 
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

  var points:List[Vector3] = List()
  var meshes:List[Mesh] = List(
    new Mesh(
      List(
        Vertex(Color(255, 255, 255), Vector3(1, 1, 1)),
        Vertex(Color(0, 255, 255), Vector3(-1, 1, 1)),
        Vertex(Color(255, 0, 255), Vector3(1, -1, 1)),
        Vertex(Color(255, 255, 0), Vector3(1, 1, -1)),
        Vertex(Color(255, 0, 0), Vector3(1, -1, -1)),
        Vertex(Color(0, 255, 0), Vector3(-1, 1, -1)),
        Vertex(Color(0, 0, 255), Vector3(-1, -1, 1)),
        Vertex(Color(0, 0, 0), Vector3(-1, -1, -1)))
      ,
      List(
        1, 0, 2,
        2, 6, 1))
      // 1, 0, 2, 6,
      // 0, 3, 4, 2,
      // 3, 5, 7, 4,
      // 5, 1, 6, 7,
      // 5, 3, 0, 1,
      // 7, 4, 2, 6
  )
  
  // 
  def setup(g: PApplet)
  {
    app = g
    app.size(width, height, PConstants.P3D)

    numFont = app.createFont("SanSerif", uiFontSize)
    uiFont = app.createFont("SanSerif", uiFontSize)

    meshes = meshes.map(_.scale(90.f))

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
  def mousePressed(mouseX:Int, mouseY:Int, mouseButton: Int)
  {
    if(state == GameStop || state == GamePlay)
    {
      if(mouseButton == PConstants.LEFT)
      {
        points = Vector3(mouseX, mouseY, 100) :: points
      }
    }
  }

  // 
  def mouseReleased(mouseX:Int, mouseY:Int, mouseButton: Int)
  {
    if(state == GameStop || state == GamePlay)
    {
      if(mouseButton == PConstants.LEFT)
      {
        points = Vector3(mouseX, mouseY, -100) :: points
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
	      case 'h' => cut(Plane(0.f, 1.f, 0.f, 0.f))
	      case 'v' => cut(Plane(1.f, 0.f, 0.f, 0.f))
	      case _ => ()
      }
    }
  }

  //
  def cut(p:Plane) =
  {
    var newMeshes = List[Mesh]()

    println("cut before----------------------------")
    for(m <- meshes)
      println(m.toString)
    
    for(m <- meshes)
      newMeshes = newMeshes ::: Mesh.cutByPlane(m, p)

    meshes = newMeshes

    println("")
    println("cut after----------------------------")
    for(m <- meshes)
      println(m.toString)
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

    // val mesh = new Mesh(
    //   List(
    //     Vertex(Color(255, 255, 255), Vector3(1, 1, 1)),
    //     Vertex(Color(0, 255, 255), Vector3(-1, 1, 1)),
    //     Vertex(Color(255, 0, 255), Vector3(1, -1, 1)),
    //     Vertex(Color(255, 255, 0), Vector3(1, 1, -1)),
    //     Vertex(Color(255, 0, 0), Vector3(1, -1, -1)),
    //     Vertex(Color(0, 255, 0), Vector3(-1, 1, -1)),
    //     Vertex(Color(0, 0, 255), Vector3(-1, -1, 1)),
    //     Vertex(Color(0, 0, 0), Vector3(-1, -1, -1)))
    //   ,
    //   List(
    //     1, 0, 2,
    //     2, 6, 1)
    //     // 1, 0, 2, 6,
    //     // 0, 3, 4, 2,
    //     // 3, 5, 7, 4,
    //     // 5, 1, 6, 7,
    //     // 5, 3, 0, 1,
    //     // 7, 4, 2, 6
    // )
    
    app.pushMatrix()     
    app.translate(width/2, height/2, -100)
        
    // app.scale(90)
    app.rotateY(PConstants.PI/12)
    app.beginShape(PConstants.TRIANGLES)

    for(m <- meshes)
    {
      for(i <- m.indecies)
      {
        val v = m.vertecies(i)
        app.fill(v.color.r, v.color.g, v.color.b)
        app.vertex(v.pos.x, v.pos.y, v.pos.z)
      }  
    }
        
    app.endShape()
    
    app.popMatrix() 
  }
}
