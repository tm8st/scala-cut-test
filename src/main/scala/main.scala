/* ------------------------------------------------------------
   !Scala+Processing 3D 切断テスト
------------------------------------------------------------ */
import processing.core._
import tm8st.math._
import tm8st.mesh._
import tm8st.util._

/* ------------------------------------------------------------
!
!@memo 
------------------------------------------------------------ */
object MeshComponent
{
}
class MeshComponent(val mesh:Mesh, var pos:Vector3, var rotation:Vector3, var speed:Vector3)
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
        1, 0, 2,
        2, 6, 1
        ))
        
      // 1, 0, 2, 6,
      // 0, 3, 4, 2,
      // 3, 5, 7, 4,
      // 5, 1, 6, 7,
      // 5, 3, 0, 1,
      // 7, 4, 2, 6
  )

  var meshComponents:List[MeshComponent] = List()
  val planePos = Vector3(width/2, height/2, -100.f)
  val planes = List(Plane.from(Vector3(0.f, 1.f, 0.f), planePos),
                    Plane.from(Vector3(1.f, 0.f, 0.f), planePos),
                    Plane.from(Vector3(1.f, 1.f, 0.f).normal(), planePos),
                    Plane.from(Vector3(-1.f, 1.f, 0.f).normal(), planePos))
  
  // 
  def setup(g: PApplet)
  {
    app = g
    app.size(width, height, PConstants.P3D)

    numFont = app.createFont("SanSerif", uiFontSize)
    uiFont = app.createFont("SanSerif", uiFontSize)

    meshes = meshes.map(_.scale(90.f))

    val meshPos = Vector3(width/2, height/2, -100.f)
    val meshRotate = Vector3(0.f, PConstants.PI/16.f, 0.f)
    meshComponents = new MeshComponent(meshes.head, meshPos, meshRotate, Vector3.Zero) :: meshComponents

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
	      case 'h' => cut(planes(0))
	      case 'v' => cut(planes(1))
        case 'l' => cut(planes(2))
        case 'k' => cut(planes(3))
        // case 'h' => cut(Plane(0.f, 1.f, 0.f, 0.f))
	      // case 'v' => cut(Plane(1.f, 0.f, 0.f, 0.f))
	      case _ => ()
      }
      // val planes = List(Plane.from(Vector3(0.f, 1.f, 0.f), Vector3(0.f, 0.f, 0.f)),
      //                   Plane.from(Vector3(1.f, 0.f, 0.f), Vector3(0.f, 0.f, 0.f)))

    }
  }

  //
  def cut(p:Plane) =
  {
    var newMeshComponents = List[MeshComponent]()
    for(mc <- meshComponents)
    {
      Logger.debug("cut before----------------------------")
      Logger.debug(mc.toString)

      // plane to mesh local. current only pos.
      val lp = Plane.from(p.n, mc.pos - p.point)

      val cutResult = Mesh.cutByPlane(mc.mesh, lp)

      if(cutResult.front == null == false)
        newMeshComponents = new MeshComponent(cutResult.front, mc.pos, mc.rotation, mc.speed + Vector3.Rand(-1.f, 1.f)) :: newMeshComponents
      if(cutResult.back == null == false)
        newMeshComponents = new MeshComponent(cutResult.back, mc.pos, mc.rotation, mc.speed + Vector3.Rand(-1.f, 1.f)) :: newMeshComponents

      Logger.debug("")
      Logger.debug("cut after----------------------------")
      Logger.debug(mc.toString)
    }

    meshComponents = newMeshComponents
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

    val cameraAt = Vector3(width/2, height/2, -100.f)
    // val carameRot = Vector3(0.f, PConstants.PI/12.f, 0.f)
    app.camera(0.0f, 0.0f, 300.0f,
               cameraAt.x, cameraAt.y, cameraAt.z,
               // 0.0f, 0.0f, -1.0f,
               0.0f, 1.0f, 0.0f)

    val time = (app.millis() / 1000 ).toString
    app.text(" time: " + time + "sec", 4, 4 + uiFontSize)

    // 移動
    for(m <- meshComponents)
    {
      m.pos += m.speed

      // 一定の範囲でうごかす
      val range = 50.f
      val x = if(m.pos.x-cameraAt.x > range || m.pos.x-cameraAt.x < -range) -m.speed.x else m.speed.x
      val y = if(m.pos.y-cameraAt.y > range || m.pos.y-cameraAt.y < -range) -m.speed.y else m.speed.y
      val z = if(m.pos.z-cameraAt.z > range || m.pos.z-cameraAt.z < -range) -m.speed.z else m.speed.z
      
      m.speed = Vector3(x, y, z)
    }    

    // テストコリジョン
    val debugView = false
    var id = 0
    if(debugView)
    {
      for(mc <- meshComponents)
      {
        for(t <- mc.mesh.triangles)
        {
          for(plane <- planes)
          {
            val (rel, ps) = Collision.calcIntersectPoints(t.v0.pos, t.v1.pos, t.v2.pos, plane)

            for(p <- ps)
            {
              app.stroke(0, 0, 0)
              app.fill(0, 0, 0)
              app.text(id + ":_" + rel + "p=" +p +"_____" + t.edgePosition(p.edgeID, p.rate), 4, 20 + uiFontSize + id*28)
              app.text("  tri=" + t.v0.pos + ",_" + t.v1.pos + ",_" + t.v2.pos, 4, 20 + uiFontSize + id*28 + 14)
        
              app.pushMatrix()

              val trans = t.edgePosition(p.edgeID, p.rate)
              app.translate(mc.pos.x + trans.pos.x, mc.pos.y + trans.pos.y, mc.pos.z + trans.pos.z)
              app.rotateY(mc.rotation.y)

              // app.stroke(32 * id, 32 * id, 32 * id)
              app.fill(32 * id, 32 * id, 32 * id)
              app.sphere(8)

              app.popMatrix()
              id += 1
            }
          }
        }  
      }
    }

    // メッシュ描画
    for(mc <- meshComponents)
    {
      val meshPos = mc.pos
      val meshRotate = mc.rotation

      app.pushMatrix()
      app.translate(meshPos.x, meshPos.y, meshPos.z)
      app.rotateY(meshRotate.y)

      app.beginShape(PConstants.TRIANGLES)
      for(i <- mc.mesh.indecies)
      {
        val v = mc.mesh.vertecies(i)
        app.stroke(0, 0, 0)
        app.fill(v.color.r, v.color.g, v.color.b)
        app.vertex(v.pos.x, v.pos.y, v.pos.z)
      }
      app.endShape()

      app.popMatrix()
    }
  }
}
