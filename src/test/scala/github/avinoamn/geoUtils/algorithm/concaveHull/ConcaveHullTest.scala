package github.avinoamn.geoUtils.algorithm.concaveHull

import org.locationtech.jts.geom.{Coordinate, GeometryFactory, LinearRing, MultiPolygon, Polygon}
import org.scalatest.funsuite.AnyFunSuite

class ConcaveHullTest extends AnyFunSuite {

  val factory: GeometryFactory = new GeometryFactory()

  val testLinearRings: Array[LinearRing] = Array(
    factory.createLinearRing(Array(
      new Coordinate(-1, -1),
      new Coordinate(2, -1),
      new Coordinate(2, 2),
      new Coordinate(-1, 2),
      new Coordinate(-1, -1)
    )),
    factory.createLinearRing(Array(
      new Coordinate(0, 0),
      new Coordinate(1, 0),
      new Coordinate(0, 1),
      new Coordinate(1, 1),
      new Coordinate(0, 0)
    ))
  )

  val resultLinearRings: Array[LinearRing] = Array(
    testLinearRings(0),
    factory.createLinearRing(Array(
      new Coordinate(0, 0),
      new Coordinate(1, 0),
      new Coordinate(0.5, 0.5),
      new Coordinate(1, 1),
      new Coordinate(0, 1),
      new Coordinate(0.5, 0.5),
      new Coordinate(0, 0)
    ))
  )

  val testPolygon: Polygon = factory.createPolygon(testLinearRings.head, testLinearRings.tail)
  val resultPolygon: Polygon = factory.createPolygon(resultLinearRings.head, resultLinearRings.tail)

  val testMultiPolygon: MultiPolygon = factory.createMultiPolygon(Array(testPolygon))
  val resultMultiPolygon: MultiPolygon = factory.createMultiPolygon(Array(resultPolygon))

  test("Should fix Linear Ring") {
    val blyaddddd = ConcaveHull.concaveHull(testLinearRings(1))
    assert(resultLinearRings(1).equals(blyaddddd))
  }

  test("Should fix polygon") {
    val blyaddddd = ConcaveHull.concaveHull(testPolygon)
    assert(resultPolygon.equals(blyaddddd))
  }

  test("Should fix multi polygon") {
    assert(resultMultiPolygon.equals(ConcaveHull.concaveHull(testMultiPolygon)))
  }
}
