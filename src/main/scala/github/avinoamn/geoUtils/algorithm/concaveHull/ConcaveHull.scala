package github.avinoamn.geoUtils.algorithm.concaveHull

import github.avinoamn.geoUtils.algorithm.concaveHull.dataStructures._
import github.avinoamn.geoUtils.algorithm.concaveHull.models.{IntersectingLine, Vertex}
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
    val fixedCoords = computeHull(coords)

    factory.createLineString(fixedCoords)
  }

  def computeHull(coords: Array[Coordinate]): Array[Coordinate] = {
    implicit val verticesMap: VerticesMap = new VerticesMap(new Vertex(coords.head.x, coords.head.y, isHead = true))
    implicit val horizontalSortedVerticesIds: HorizontalSortedVerticesIds = new HorizontalSortedVerticesIds(verticesMap.getHead.id)

    coords.slice(1, coords.length).zipWithIndex.foreach{ case (coord, index) =>
      val vertex = new Vertex(coord.x, coord.y, isTail = (index == coords.length - 2))
      val prevVertex = verticesMap.get(horizontalSortedVerticesIds.getCurrentIndexVertexId)
      val slope = Equations.getSlope(prevVertex, vertex)

      verticesMap.add(vertex)

      if (vertex.x >= prevVertex.x) {
        val intersectingLines = horizontalSortedVerticesIds.getLTRIntersectingLines(prevVertex, vertex, slope)
        fixLTRIntersections(intersectingLines, prevVertex, vertex, slope)
      } else if (vertex.x < prevVertex.x) {
        val intersectingLines = horizontalSortedVerticesIds.getRTLIntersectingLines(vertex, prevVertex, slope)
        fixRTLIntersections(intersectingLines, vertex, prevVertex, slope)
      }
    }

    /** Return the fixed geometry's coordinates. */
    verticesMap.getOrdered.map(vertex => new Coordinate(vertex.x, vertex.y)).toArray
  }

  /** Fix Intersection points in the geometry by going around them.
   *
   * @param intersectingLines List of lines that are intersecting with the line of the vertex that was just sorted in.
   * @param firstLeftVertex Previous sorted vertex.
   * @param rightVertex Latest sorted vertex.
   * @param slope Slope of the line of the vertex that was just sorted in.
   */
  private def fixLTRIntersections(intersectingLines: List[IntersectingLine], firstLeftVertex: Vertex, rightVertex: Vertex, slope: Double)
                                 (implicit verticesMap: VerticesMap, horizontalSortedVerticesIds: HorizontalSortedVerticesIds): Unit = {
    /** Sort the intersections from left to right. */
    val sortedIntersectionLines = intersectingLines.sortBy(line => {
      slope > 0 match {
        case true => (line.intersection.x, line.intersection.y)
        case false => (line.intersection.x, -1 * line.intersection.y)
      }
    })

    var leftVertex = firstLeftVertex
    sortedIntersectionLines.foreach(line => {
      val headAndTail = line.getHeadAndTail
      val headVertex = verticesMap.get(headAndTail.head)
      val tailVertex = verticesMap.get(headAndTail.tail)

      val reverseFrom = tailVertex.id
      val reverseTo = leftVertex.id
      verticesMap.reverse(reverseFrom, reverseTo)

      val headFixVertex = new Vertex(line.intersection.x, line.intersection.y)
      verticesMap.add(headFixVertex)

      horizontalSortedVerticesIds.addLTRIntersectionVertex(headFixVertex)
      headVertex.replaceNext(tailVertex, headFixVertex, line.slope, tailVertex.id == line.left)
      headFixVertex.setLeftNext(leftVertex, slope)

      val tailFixVertex = new Vertex(line.intersection.x, line.intersection.y)
      verticesMap.add(tailFixVertex)

      horizontalSortedVerticesIds.addLTRIntersectionVertex(tailFixVertex)
      tailVertex.setNext(tailFixVertex, line.slope, tailVertex.id == line.left)

      leftVertex = tailFixVertex
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
                                 (implicit verticesMap: VerticesMap, horizontalSortedVerticesIds: HorizontalSortedVerticesIds): Unit = {
    /** Sort the intersections from right to left. */
    val sortedIntersectionLines = intersectingLines.sortBy(line => {
      slope > 0 match {
        case true => (-1 * line.intersection.x, -1 * line.intersection.y)
        case false => (-1 * line.intersection.x, line.intersection.y)
      }
    })

    var rightVertex = firstRightVertex
    sortedIntersectionLines.foreach(line => {
      val headAndTail = line.getHeadAndTail
      val headVertex = verticesMap.get(headAndTail.head)
      val tailVertex = verticesMap.get(headAndTail.tail)

      val reverseFrom = tailVertex.id
      val reverseTo = rightVertex.id
      verticesMap.reverse(reverseFrom, reverseTo)

      val headFixVertex = new Vertex(line.intersection.x, line.intersection.y)
      verticesMap.add(headFixVertex)

      horizontalSortedVerticesIds.addRTLIntersectionVertex(headFixVertex)
      headVertex.replaceNext(tailVertex, headFixVertex, line.slope, tailVertex.id == line.left)
      headFixVertex.setRightNext(rightVertex, slope)

      val tailFixVertex = new Vertex(line.intersection.x, line.intersection.y)
      verticesMap.add(tailFixVertex)

      horizontalSortedVerticesIds.addRTLIntersectionVertex(tailFixVertex)
      tailVertex.setNext(tailFixVertex, line.slope, tailVertex.id == line.left)

      rightVertex = tailFixVertex
    })
    rightVertex.setLeftNext(leftVertex, slope)
  }
}
