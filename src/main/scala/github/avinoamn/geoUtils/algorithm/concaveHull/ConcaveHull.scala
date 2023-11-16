package github.avinoamn.geoUtils.algorithm.concaveHull

import github.avinoamn.geoUtils.algorithm.concaveHull.dataStructures._
import github.avinoamn.geoUtils.algorithm.concaveHull.models.{IntersectingLine, Line, Vertex}
import github.avinoamn.geoUtils.algorithm.concaveHull.types.IntersectingLineTypes
import github.avinoamn.geoUtils.algorithm.concaveHull.utils.math.Equations
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
    val fixedCoords = concaveHull(coords)

    factory.createLineString(fixedCoords)
  }

  def concaveHull(coordinates: Array[Coordinate]): Array[Coordinate] = {
    implicit val linkedVertices: LinkedVertices = new LinkedVertices(coordinates)
    implicit val horizontalSortedVertices: HorizontalSortedVertices = new HorizontalSortedVertices(linkedVertices.head)

    computeHull

    /** Return the fixed geometry's coordinates. */
    linkedVertices.getOrdered.map(vertex => new Coordinate(vertex.x, vertex.y)).toArray
  }

  /** Loops over the geometry's vertices, while finding and fixing it's self intersections. */
  private def computeHull(implicit linkedVertices: LinkedVertices, horizontalSortedVertices: HorizontalSortedVertices): Unit = {
    linkedVertices.fold((prevVertex, vertex) => {
      val iterationLine = Line(prevVertex, vertex, Equations.getSlope(prevVertex, vertex))

      val intersectingLines = horizontalSortedVertices.getIntersectingLines(iterationLine)

      fixIntersections(iterationLine, intersectingLines)
    })
  }

  /** Fix all intersections for the current iteration line. */
  private def fixIntersections(iterationLine: Line, intersectingLines: List[IntersectingLine])
                              (implicit horizontalSortedVertices: HorizontalSortedVertices, linkedVertices: LinkedVertices): Unit = {
    val sortedIntersectingLines = sortIntersectingLines(intersectingLines, iterationLine.slope, iterationLine.isLTR)

    val updatedIterationLine = sortedIntersectingLines.foldLeft(iterationLine)((iterationLine, intersectingLine) => {
      val (intersectionFixHead, intersectionFixTail) = fixIntersection(iterationLine, intersectingLine)

      horizontalSortedVertices.addIntersectionVertices(intersectionFixHead, intersectionFixTail)

      if (iterationLine.isLTR) {
        handleLTRIntersections(intersectingLine, intersectionFixHead, intersectionFixTail)
      } else {
        handleRTLIntersections(intersectingLine, intersectionFixHead, intersectionFixTail)
      }

      iterationLine.copy(head = intersectionFixTail)
    })

    linkedVertices.setNeighbors(updatedIterationLine.head, updatedIterationLine.tail, updatedIterationLine.slope)
  }

  /** Sort the intersecting lines by their intersection coordinate's `x` value. */
  private def sortIntersectingLines(intersectingLines: List[IntersectingLine], slope: Double, isLTR: Boolean): List[IntersectingLine] = {
    val mult = if (isLTR) 1 else -1

    intersectingLines.sortBy(line => {
      if (slope > 0) {
        (mult * line.intersection.x, mult * line.intersection.y)
      } else {
        (mult * line.intersection.x, mult * line.intersection.y * -1)
      }
    })
  }

  /** Gets the intersection vertex. */
  private def getIntersectionVertex(coordinate: Coordinate): Vertex = {
    // TODO: Add buffer

    new Vertex(coordinate.x, coordinate.y)
  }

  /** Fix intersection between two lines. */
  private def fixIntersection(iterationLine: Line, intersectingLine: IntersectingLine)
                             (implicit linkedVertices: LinkedVertices): (Vertex, Vertex) = {
    val intersectionFixTail = getIntersectionVertex(intersectingLine.intersection)
    linkedVertices.replace(intersectingLine.head, intersectionFixTail, intersectingLine.slope)

    val intersectionFixHead = getIntersectionVertex(intersectingLine.intersection)
    linkedVertices.replaceNext(iterationLine.head, intersectionFixHead, iterationLine.slope)

    linkedVertices.reverse(intersectionFixTail, intersectionFixHead)

    linkedVertices.setNext(intersectingLine.head, intersectionFixHead, intersectingLine.slope)
    intersectionFixTail.setNext(iterationLine.tail)

    (intersectionFixHead, intersectionFixTail)
  }

  /** Handle intersection lines by their specific type and direction (LTR). */
  private def handleLTRIntersections(intersectingLine: IntersectingLine, intersectionFixHead: Vertex, intersectionFixTail: Vertex)
                                    (implicit horizontalSortedVertices: HorizontalSortedVertices): Unit = {
    intersectingLine.`type` match {
      case IntersectingLineTypes.Middle => {
        /** If exist, removes old middle, and insert a new and shorted one from the intersection vertex. */
        if (horizontalSortedVertices.isMiddleExist(intersectingLine.left, intersectingLine.right)) {
          horizontalSortedVertices.removeMiddle(intersectingLine.left, intersectingLine.right)

          if (intersectingLine.head == intersectingLine.right) {
            horizontalSortedVertices.addMiddle(intersectionFixHead, intersectingLine.right, intersectingLine.slope)
          } else {
            horizontalSortedVertices.addMiddle(intersectionFixTail, intersectingLine.right, intersectingLine.slope)
          }
        }
      }

      case _ =>
    }
  }

  /** Handle intersection lines by their specific type and direction (RTL). */
  private def handleRTLIntersections(intersectingLine: IntersectingLine, intersectionFixHead: Vertex, intersectionFixTail: Vertex)
                                    (implicit horizontalSortedVertices: HorizontalSortedVertices): Unit = {
    intersectingLine.`type` match {
      /** If exist, removes old middle, and insert a new and shorted one from the intersection vertex. */
      case IntersectingLineTypes.Middle => {
        if (horizontalSortedVertices.isMiddleExist(intersectingLine.left, intersectingLine.right)) {
          horizontalSortedVertices.removeMiddle(intersectingLine.left, intersectingLine.right)

          if (intersectingLine.head == intersectingLine.left) {
            horizontalSortedVertices.addMiddle(intersectingLine.left, intersectionFixHead, intersectingLine.slope)
          } else {
            horizontalSortedVertices.addMiddle(intersectingLine.left, intersectionFixTail, intersectingLine.slope)
          }
        }
      }

      case _ =>
    }
  }
}
