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
class TestSuite extends FunSuite
{
  test("""List functions""")
  {
    println(List(1, 3, 4).filter(_ % 2 == 0))
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
