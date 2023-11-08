package github.avinoamn.geoUtils.algorithm.concaveHull

import github.avinoamn.geoUtils.algorithm.concaveHull.dataStructures._
import github.avinoamn.geoUtils.algorithm.concaveHull.models.{IntersectingLine, Vertex}
import github.avinoamn.geoUtils.algorithm.concaveHull.types.IntersectingLineTypes
import github.avinoamn.geoUtils.algorithm.concaveHull.utils.math.Equations
import github.avinoamn.geoUtils.algorithm.concaveHull.utils.models.LinkedVertices
import org.locationtech.jts.geom._

object ConcaveHull {
  implicit val factory: GeometryFactory = new GeometryFactory()

  def concaveHull(multiPolygon: MultiPolygon): MultiPolygon = {
    val fixedPolygons = (0 until multiPolygon.getNumGeometries)
      .map(multiPolygon.getGeometryN(_).asInstanceOf[Polygon])
      .map(concaveHull)
      .toArray

    factory.createMultiPolygon(fixedPolygons)
  }

  def concaveHull(polygon: Polygon): Polygon = {
    val boundary = polygon.getBoundary
    val fixedLinearRings = (0 until boundary.getNumGeometries)
      .map(boundary.getGeometryN(_).asInstanceOf[LinearRing])
      .map(concaveHull)
      .toArray

    if (fixedLinearRings.length == 1) factory.createPolygon(fixedLinearRings.head)
    else if (fixedLinearRings.length > 1) factory.createPolygon(fixedLinearRings.head, fixedLinearRings.tail)
    else polygon
  }

  def concaveHull(multiLineString: MultiLineString): MultiLineString = {
    val fixedLineStrings = (0 until multiLineString.getNumGeometries)
      .map(multiLineString.getGeometryN(_).asInstanceOf[LineString])
      .map(concaveHull)
      .toArray

    factory.createMultiLineString(fixedLineStrings)
  }

  def concaveHull(linearRing: LinearRing): LinearRing = {
    val lineString = factory.createLineString(linearRing.getCoordinateSequence)
    val fixedLineString = concaveHull(lineString)

    factory.createLinearRing(fixedLineString.getCoordinateSequence)
  }

  def concaveHull(lineString: LineString): LineString = {
    val coords = lineString.getCoordinates
    val fixedCoords = computeHull(coords)

    factory.createLineString(fixedCoords)
  }

  def computeHull(coords: Array[Coordinate]): Array[Coordinate] = {
    val headVertex = new Vertex(coords.head.x, coords.head.y, isHead = true)
    implicit val horizontalSortedVertices: HorizontalSortedVertices = new HorizontalSortedVertices(headVertex)

    coords.slice(1, coords.length).zipWithIndex.foreach{ case (coord, index) =>
      val vertex = new Vertex(coord.x, coord.y, isTail = (index == coords.length - 2))
      val prevVertex = horizontalSortedVertices.getCurrentIndexVertex
      val slope = Equations.getSlope(prevVertex, vertex)

      if (vertex.x >= prevVertex.x) {
        val intersectingLines = horizontalSortedVertices.getLTRIntersectingLines(prevVertex, vertex, slope)
        fixLTRIntersections(intersectingLines, prevVertex, vertex, slope)
      } else if (vertex.x < prevVertex.x) {
        val intersectingLines = horizontalSortedVertices.getRTLIntersectingLines(vertex, prevVertex, slope)
        fixRTLIntersections(intersectingLines, vertex, prevVertex, slope)
      }
    }

    /** Return the fixed geometry's coordinates. */
    LinkedVertices.getOrdered(headVertex).map(vertex => new Coordinate(vertex.x, vertex.y)).toArray
  }

  /** Fix Intersection points in the geometry by going around them.
   *
   * @param intersectingLines List of lines that are intersecting with the line of the vertex that was just sorted in.
   * @param firstLeftVertex Previous sorted vertex.
   * @param rightVertex Latest sorted vertex.
   * @param slope Slope of the line of the vertex that was just sorted in.
   */
  private def fixLTRIntersections(intersectingLines: List[IntersectingLine], firstLeftVertex: Vertex, rightVertex: Vertex, slope: Double)
                                 (implicit horizontalSortedVertices: HorizontalSortedVertices): Unit = {
    /** Sort the intersections from left to right. */
    val sortedIntersectionLines = intersectingLines.sortBy(line => {
      if (slope > 0) {
        (line.intersection.x, line.intersection.y)
      } else {
        (line.intersection.x, -1 * line.intersection.y)
      }
    })

    var leftVertex = firstLeftVertex
    sortedIntersectionLines.foreach(line => {
      val headAndTail = line.getHeadAndTail
      val headVertex = headAndTail.head
      val tailVertex = headAndTail.tail

      val reverseFrom = tailVertex
      val reverseTo = leftVertex
      LinkedVertices.reverse(reverseFrom, reverseTo)

      val headFixVertex = new Vertex(line.intersection.x, line.intersection.y)
      horizontalSortedVertices.addLTRIntersectionVertex(headFixVertex)
      headVertex.replaceNext(tailVertex, headFixVertex, tailVertex == line.left)
      headFixVertex.setLeftNext(leftVertex, slope)

      val tailFixVertex = new Vertex(line.intersection.x, line.intersection.y)
      horizontalSortedVertices.addLTRIntersectionVertex(tailFixVertex)
      tailVertex.setNext(tailFixVertex, line.slope, tailVertex == line.left)

      leftVertex = tailFixVertex

      line.`type` match {
        case IntersectingLineTypes.Middle => {
          /** If exist, removes old middle, and insert a new and shorted one from the intersection vertex. */
          if (horizontalSortedVertices.isMiddleExist(line.left, line.right)) {
            horizontalSortedVertices.removeMiddle(line.left, line.right)

            if (headVertex == line.right) {
              horizontalSortedVertices.addMiddle(headFixVertex, headVertex, line.slope)
            } else {
              horizontalSortedVertices.addMiddle(tailFixVertex, tailVertex, line.slope)
            }
          }
        }

        case _ =>
      }
    })
    leftVertex.setRightNext(rightVertex, slope)
  }

  /** Fix Intersection points in the geometry by going around them.
   *
   * @param intersectingLines List of lines that are intersecting with the line of the vertex that was just sorted in.
   * @param leftVertex Latest sorted vertex.
   * @param firstRightVertex Previous sorted vertex.
   * @param slope Slope of the line of the vertex that was just sorted in.
   */
  private def fixRTLIntersections(intersectingLines: List[IntersectingLine], leftVertex: Vertex, firstRightVertex: Vertex, slope: Double)
                                 (implicit horizontalSortedVertices: HorizontalSortedVertices): Unit = {
    /** Sort the intersections from right to left. */
    val sortedIntersectionLines = intersectingLines.sortBy(line => {
      if (slope > 0) {
        (-1 * line.intersection.x, -1 * line.intersection.y)
      } else {
        (-1 * line.intersection.x, line.intersection.y)
      }
    })

    var rightVertex = firstRightVertex
    sortedIntersectionLines.foreach(line => {
      val headAndTail = line.getHeadAndTail
      val headVertex = headAndTail.head
      val tailVertex = headAndTail.tail

      val reverseFrom = tailVertex
      val reverseTo = rightVertex
      LinkedVertices.reverse(reverseFrom, reverseTo)

      val headFixVertex = new Vertex(line.intersection.x, line.intersection.y)
      horizontalSortedVertices.addRTLIntersectionVertex(headFixVertex)
      headVertex.replaceNext(tailVertex, headFixVertex, tailVertex.id == line.left.id)
      headFixVertex.setRightNext(rightVertex, slope)

      val tailFixVertex = new Vertex(line.intersection.x, line.intersection.y)
      horizontalSortedVertices.addRTLIntersectionVertex(tailFixVertex)
      tailVertex.setNext(tailFixVertex, line.slope, tailVertex.id == line.left.id)

      rightVertex = tailFixVertex

      line.`type` match {
        /** If exist, removes old middle, and insert a new and shorted one from the intersection vertex. */
        case IntersectingLineTypes.Middle => {
          if (horizontalSortedVertices.isMiddleExist(line.left, line.right)) {
            horizontalSortedVertices.removeMiddle(line.left, line.right)

            if (headVertex == line.left) {
              horizontalSortedVertices.addMiddle(headVertex, headFixVertex, line.slope)
            } else {
              horizontalSortedVertices.addMiddle(tailVertex, tailFixVertex, line.slope)
            }
          }
        }

        case _ =>
      }
    })
    rightVertex.setLeftNext(leftVertex, slope)
  }
}
