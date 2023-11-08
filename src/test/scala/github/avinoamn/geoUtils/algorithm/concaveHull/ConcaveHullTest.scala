package github.avinoamn.geoUtils.algorithm.concaveHull

import org.locationtech.jts.geom.{Coordinate, GeometryFactory, LinearRing, MultiPolygon, Polygon}
import org.scalatest.funsuite.AnyFunSuite

class ConcaveHullTest extends AnyFunSuite {

  val factory: GeometryFactory = new GeometryFactory()

  test("Valid Linear Ring (should stay the same") {
    val linearRing = factory.createLinearRing(Array(
      new Coordinate(-1, -1),
      new Coordinate(2, -1),
      new Coordinate(2, 2),
      new Coordinate(-1, 2),
      new Coordinate(-1, -1)
    ))

    val fixedLinearRing = ConcaveHull.concaveHull(linearRing)

    assert(linearRing.equals(fixedLinearRing))
  }

  test("Classic hourglass Linear Ring") {
    val linearRing = factory.createLinearRing(Array(
      new Coordinate(0, 0),
      new Coordinate(1, 0),
      new Coordinate(0, 1),
      new Coordinate(1, 1),
      new Coordinate(0, 0)
    ))

    val expectedFixedLinearRing = factory.createLinearRing(Array(
      new Coordinate(0, 0),
      new Coordinate(1, 0),
      new Coordinate(0.5, 0.5),
      new Coordinate(1, 1),
      new Coordinate(0, 1),
      new Coordinate(0.5, 0.5),
      new Coordinate(0, 0)
    ))

    val fixedLinearRing = ConcaveHull.concaveHull(linearRing)

    assert(expectedFixedLinearRing.equals(fixedLinearRing))
  }

  test("4 intersections onat once") {
    val linearRing = factory.createLinearRing(Array(
      new Coordinate(14, 4),
      new Coordinate(0, 4),
      new Coordinate(0, 10),
      new Coordinate(12, 10),
      new Coordinate(12, 8),
      new Coordinate(2, 8),
      new Coordinate(2, 6),
      new Coordinate(14, 6),
      new Coordinate(14, 14),
      new Coordinate(0, 0),
      new Coordinate(14, 0),
      new Coordinate(14, 4)
    ))

    val expectedFixedLinearRing = factory.createLinearRing(Array(
      new Coordinate(14, 4),
      new Coordinate(4, 4),
      new Coordinate(6, 6),
      new Coordinate(2, 6),
      new Coordinate(2, 8),
      new Coordinate(8, 8),
      new Coordinate(10, 10),
      new Coordinate(12, 10),
      new Coordinate(12, 8),
      new Coordinate(8, 8),
      new Coordinate(6, 6),
      new Coordinate(14, 6),
      new Coordinate(14, 14),
      new Coordinate(10, 10),
      new Coordinate(0, 10),
      new Coordinate(0, 4),
      new Coordinate(4, 4),
      new Coordinate(0, 0),
      new Coordinate(14, 0),
      new Coordinate(14, 4)
    ))

    val fixedLinearRing = ConcaveHull.concaveHull(linearRing)

    assert(expectedFixedLinearRing.equals(fixedLinearRing))
  }

  test("Double hourglass") {
    val linearRing = factory.createLinearRing(Array(
      new Coordinate(1, 1),
      new Coordinate(2, 1),
      new Coordinate(2, 7),
      new Coordinate(3, 7),
      new Coordinate(3, 6),
      new Coordinate(1, 4),
      new Coordinate(3, 4),
      new Coordinate(1, 2),
      new Coordinate(1, 1)
    ))

    val expectedFixedLinearRing = factory.createLinearRing(Array(
      new Coordinate(1, 1),
      new Coordinate(2, 1),
      new Coordinate(2, 3),
      new Coordinate(3, 4),
      new Coordinate(2, 4),
      new Coordinate(2, 5),
      new Coordinate(3, 6),
      new Coordinate(3, 7),
      new Coordinate(2, 7),
      new Coordinate(2, 5),
      new Coordinate(1, 4),
      new Coordinate(2, 4),
      new Coordinate(2, 3),
      new Coordinate(1, 2),
      new Coordinate(1, 1)
    ))

    val fixedLinearRing = ConcaveHull.concaveHull(linearRing)

    assert(expectedFixedLinearRing.equals(fixedLinearRing))
  }

  test("Complex 4 intersection squares") {
    val linearRing = factory.createLinearRing(Array(
      new Coordinate(0, -1),
      new Coordinate(6, -1),
      new Coordinate(6, 3),
      new Coordinate(4, 3),
      new Coordinate(4, 1),
      new Coordinate(1, 1),
      new Coordinate(1, 3),
      new Coordinate(3, 3),
      new Coordinate(3, 0),
      new Coordinate(2, 0),
      new Coordinate(2, 2),
      new Coordinate(5, 2),
      new Coordinate(5, 4),
      new Coordinate(0, 4),
      new Coordinate(0, -1)
    ))

    val expectedFixedLinearRing = factory.createLinearRing(Array(
      new Coordinate(0, -1),
      new Coordinate(6, -1),
      new Coordinate(6, 3),
      new Coordinate(5, 3),
      new Coordinate(5, 2),
      new Coordinate(4, 2),
      new Coordinate(4, 1),
      new Coordinate(3, 1),
      new Coordinate(3, 2),
      new Coordinate(2, 2),
      new Coordinate(2, 1),
      new Coordinate(3, 1),
      new Coordinate(3, 0),
      new Coordinate(2, 0),
      new Coordinate(2, 1),
      new Coordinate(1, 1),
      new Coordinate(1, 3),
      new Coordinate(3, 3),
      new Coordinate(3, 2),
      new Coordinate(4, 2),
      new Coordinate(4, 3),
      new Coordinate(5, 3),
      new Coordinate(5, 4),
      new Coordinate(0, 4),
      new Coordinate(0, -1)
    ))

    val fixedLinearRing = ConcaveHull.concaveHull(linearRing)

    assert(expectedFixedLinearRing.equals(fixedLinearRing))
  }
}
