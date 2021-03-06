/* ------------------------------------------------------------
 !Test codes.
 ------------------------------------------------------------ */

import org.scalatest.FunSuite

import tm8st.math._
import tm8st.mesh._

/* ------------------------------------------------------------
   !
   !@memo
------------------------------------------------------------ */
class TMTestSuite extends FunSuite
{
  test("""List functions""")
  {
    println(List(1, 3, 4).filter(_ % 2 == 0))
  }

  test("""Vector3 Basic""")
  {
    val a = Vector3(1.f ,0.f ,0.f)
    val b = Vector3(0.f, 1.f, 0.f)

    expect(Vector3(1.f, 1.f, 0.f)){ a + b }
    expect(Vector3(1.f, -1.f, 0.f)){ a - b }
    expect(Vector3(-1.f, -1.f, 0.f)){ (Vector3.Zero-a) - b }
    expect(Vector3(0.f, 0.f, 0.f)){ a * b }
    expect(a){ Math.lerp(a, b, 0.f) }
    expect(b){ Math.lerp(a, b, 1.f) }
    expect(b){ Math.lerp(b, a, 0.f) }
    expect(Vector3(0.5f, 0.5f, 0.f)){ Math.lerp(a, b, 0.5f) }
  }
  
  test("""Vector3 Dot""")
  {
    val d = Vector3(1.f ,0.f ,0.f) | Vector3(0.f, 1.f, 0.f)
    println(d)
    assert(d == 0.f)
  }
  test("""Vector3 Cross""")
  {
    val c = Vector3(0.f, 1.f, 0.f) ^ Vector3(1.f ,0.f ,0.f)
    println(c)
    assert(c == Vector3(0.f,0.f,1.f))
    val c2 = Vector3(1.f, 0.f, 0.f) ^ Vector3(0.f, 1.f, 0.f)
    println(c)
    assert(c2 == Vector3(0.f, 0.f, -1.f))
  }
  test("""Plane from""")
  {
    val p = Plane.from(Vector3(0.f, 1.f, 0.f), Vector3(1.f, 1.f, 0.f), Vector3(1.f, 1.f, 1.f))
    println(p)
    assert(p == Plane(0.f, 1.f, 0.f, -1.f))
  }
  test("""Plane from position and normal""")
  {
    val p = Plane.from(Vector3(0.f, 1.f, 0.f), Vector3(0.f, 3.f, 0.f))
    println(p)
    assert(p.n == Vector3(0.f, 1.f, 0.f))
    assert(p.point == Vector3(0.f, 3.f, 0.f))
  }
  test("""Plane vs Triangle""")
  {
    val p1 = Plane.from(Vector3(0.f, 1.f, 0.f), Vector3(1.f, 1.f, 0.f), Vector3(1.f, 1.f, 1.f))
    val p2 = Plane.from(Vector3(0.f, 0.f, 1.f), Vector3(0.f, 0.f, -1.f), Vector3(0.f, 1.f, 0.f))
    val t = Triangle(Vector3(0.f, 0.f, 0.f), Vector3(0.f, 3.f, 0.f), Vector3(0.f, 3.f, 3.f))

    println(p1)
    println(p2)
    println(t)

    val (rel1, ps1) = Collision.calcIntersectPoints(t, p1)
    println(ps1)
    val (rel2, ps2) = Collision.calcIntersectPoints(t, p2)
    println(ps2)
    
    assert(ps1.length == 2)
    assert(ps2.length == 0)
  }
  test("""Vertex""")
  {
    val a = Vertex(Color(0, 0, 1.f), Vector3(1.f, 0.f, 0.f))
    val b = Vertex(Color(0, 0, 0.f), Vector3(0.f, 0.f, 0.f))
    println(a)
    println(b)
    println(Vertex.lerp(a, b, 0.f))
    println(Vertex.lerp(a, b, 1.f))
    println(Vertex.lerp(a, b, 0.5f))
    
    // assert(p == Plane(0.f, 1.f, 0.f, -1.f))
  }
  test("""Plane vs Triangle 2""")
  {
    val p1 = Plane.from(Vector3(0.f, 1.f, 0.f), Vector3(1.f, 1.f, 1.f))
    val t = Triangle(Vector3(0.f, 0.f, 0.f), Vector3(0.f, 3.f, 0.f), Vector3(0.f, 3.f, 3.f))

    println(p1)
    println(t)

    val (rel, ps) = Collision.calcIntersectPoints(t, p1)
    expect(PlaneRelation.Intersect){ rel }
    println(ps)
    assert(ps(0).edgeID == 0 || ps(1).edgeID == 0)
    assert(ps(0).edgeID == 2 || ps(1).edgeID == 2)
  }
  test("""Plane vs Triangle 3""")
  {
    val p1 = Plane.from(Vector3(0.f, 1.f, 0.f), Vector3(0.f, 0.f, 0.f))
    val t = Triangle(Vector3(-6.f, 0.f, 0.f), Vector3(6.f, 0.f, 0.f), Vector3(0.f,6.f, 0.f))

    println(p1)
    println(t)

    val (rel, ps) = Collision.calcIntersectPoints(t, p1)
    expect(PlaneRelation.Intersect){ rel }
    for(p <- ps)
    {
      println(p + ", " + t.edgePosition(p.edgeID, p.rate) + ", " + t.v0 + ", " + t.v1)
    }

    assert(ps(0).edgeID == 0 || ps(1).edgeID == 0)
    assert(ps(0).edgeID == 1 || ps(1).edgeID == 1)
  }
  
  test("""Plane vs Triangle 4""")
  {
    // val p1 = Plane.from(Vector3(1.f, 0.f, 0.f), Vector3(0.f, 0.f, 0.f))
    val p1 = Plane.from(Vector3(0.f, 1.f, 0.f), Vector3(0.f, 0.f, 0.f))
    val t = Triangle(Vector3(1, -1, 1), Vector3(-1, -1, 1), Vector3(-1, 1, 1))

    println(p1)
    println(t)

    val (rel, ps) = Collision.calcIntersectPoints(t, p1)
    expect(PlaneRelation.Intersect){ rel }
    for(p <- ps)
    {
      println(p + ", " + t.edgePosition(p.edgeID, p.rate) + ", " + t.v0 + ", " + t.v1)
    }

    assert(ps(0).edgeID == 1 || ps(1).edgeID == 1)
    assert(ps(0).edgeID == 2 || ps(1).edgeID == 2)
  }

  test("""Vector3 lerp""")
  {
    val v = Math.lerp(Vector3(1.0f, -1.0f, 1.0f), Vector3(-1.0f, -1.0f, 1.0f), 0.5f)
    assert(v == Vector3(0.f, -1.0f, 1.0f))
  }

  test("""Vertex lerp""")
  {
    val v = Vertex.lerp(Vertex(Color.Red, Vector3(1.0f, -1.0f, 1.0f)),
                      Vertex(Color.Red, Vector3(-1.0f, -1.0f, 1.0f)), 0.5f)

    val v2 = Vertex.lerp(Vertex(Color.Red, Vector3(1.0f, -1.0f, 1.0f)),
                      Vertex(Color.Red, Vector3(-1.0f, -1.0f, 1.0f)), 0.5f)

    assert(v.pos == Vector3(0.f, -1.0f, 1.0f))
  }

  test("""Triangle Normalize""")
  {
    val a = Vector3(-1.f, 1.f, 1.f)
    val b = Vector3(1.f, 1.f, 1.f)
    val c = Vector3(1.f, -1.f, 1.f)

    val e0 = b-a
    val e1 = c-b

    println(e1 | e0)

    val e2 = c-a
    val e3 = b-c
    println(e2 | e3)

    // assert(p == Plane(0.f, 1.f, 0.f, -1.f))
  }
  test("""Or""")
  {
    var test = 0
    if(false || true)
      test = 1
    
    expect(1){ test }
  }
}
