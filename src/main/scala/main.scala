/* ------------------------------------------------------------
   !Scala+Processing 3D 切断テスト
------------------------------------------------------------ */
import processing.core._
import tm8st.math._
import tm8st.mesh._

/* ------------------------------------------------------------
!
!@memo 
------------------------------------------------------------ */
class MeshComponent(val mesh:Mesh, var pos:Vector3, var rotate:Vector3)
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
        1, 0, 2
        // 2, 6, 1
        ))
        
      // 1, 0, 2, 6,
      // 0, 3, 4, 2,
      // 3, 5, 7, 4,
      // 5, 1, 6, 7,
      // 5, 3, 0, 1,
      // 7, 4, 2, 6
  )

  var meshComponents:List[MeshComponent] = List()
  
  // 
  def setup(g: PApplet)
  {
    app = g
    app.size(width, height, PConstants.P3D)

    numFont = app.createFont("SanSerif", uiFontSize)
    uiFont = app.createFont("SanSerif", uiFontSize)

    meshes = meshes.map(_.scale(90.f))

    val meshPos = Vector3(width/2, height/2, -100.f)
    val meshRotate = Vector3(0.f, 0.f, 0.f)
    meshComponents = new MeshComponent(meshes.head, meshPos, meshRotate) :: meshComponents

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
    {
      println(m.toString)
    }
    
    for(m <- meshes)
    {
      newMeshes = newMeshes ::: Mesh.cutByPlane(m, p)
    }

    meshes = newMeshes

    println("")
    println("cut after----------------------------")
    for(m <- meshes)
    {
      println(m.toString)
    }
  }

  // 
  def draw()
  {
    // 3D rendering
    app.background(188)
    app.noLights()

    // draw Header
    app.textFont(uiFont)
    app.stroke(0)
    app.fill(0)

    val time = (app.millis() / 1000 ).toString
    app.text(" time: " + time + "sec", 4, 4 + uiFontSize)

    val meshPos = Vector3(width/2, height/2, -100.f)
    val meshRotate = Vector3(0.f, 0.f, 0.f)
    // val meshRotate = Vector3(0.f, PConstants.PI/12, 0.f)

    var id = 0
    for(m <- meshes)
    {
      // val meshPos = m.pos
      // val meshRotate = m.rotate
      
      for(t <- m.triangles)
      {
        val planes = List(Plane.from(Vector3(0.f, 1.f, 0.f), Vector3(0.f, 0.f, 0.f)),
                        Plane.from(Vector3(1.f, 0.f, 0.f), Vector3(0.f, 0.f, 0.f)))
        for(plane <- planes)
        {
          val (rel, ps) = Collision.calcIntersectPoints(t.v0.pos, t.v1.pos, t.v2.pos, plane)
        
          for(p <- ps)
          {
            app.stroke(0, 0, 0)
            app.fill(0, 0, 0)
            app.text(id + ": " + rel + "p=" + p, 4, 20 + uiFontSize + id*24)
            app.text("  tri=" + t, 4, 20 + uiFontSize + id*24 + 12)
            
            app.pushMatrix()

            val trans = t.edgePosition(p.edgeID, p.rate)
            app.translate(meshPos.x + trans.pos.x, meshPos.y + trans.pos.y, meshPos.z + trans.pos.z)
            app.rotateY(meshRotate.y)

            // app.stroke(32 * id, 32 * id, 32 * id)
            app.fill(32 * id, 32 * id, 32 * id)
            app.sphere(8)

            app.popMatrix()
            id += 1
          }
        }
      }  
    }

    app.stroke(0, 0, 0)
    
    for(m <- meshes)
    {
      // val meshPos = m.pos
      // val meshRotate = m.rotate

      app.pushMatrix()
      app.translate(meshPos.x, meshPos.y, meshPos.z)
      app.rotateY(meshRotate.y)
      app.beginShape(PConstants.TRIANGLES)

      for(i <- m.indecies)
      {
        val v = m.vertecies(i)
        app.fill(v.color.r, v.color.g, v.color.b)
        app.vertex(v.pos.x, v.pos.y, v.pos.z)
      }

      app.endShape()
      app.popMatrix()     
    }
  }
}
