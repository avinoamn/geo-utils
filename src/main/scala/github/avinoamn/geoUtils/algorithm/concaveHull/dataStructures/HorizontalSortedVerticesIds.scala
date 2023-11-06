package github.avinoamn.geoUtils.algorithm.concaveHull.dataStructures

import github.avinoamn.geoUtils.algorithm.concaveHull.models.{IntersectingLine, Middle, Vertex}
import github.avinoamn.geoUtils.algorithm.concaveHull.utils.math.Equations
import org.locationtech.jts.geom.{Coordinate, GeometryFactory}

/** Based on the `ActionSortedList` data structure,
 * this class holds an array of vertices ids, sorted by their vertex's `x` value.
 * During the sorting process, an action that tries to find intersecting lines
 * is performed against every vertex that passes by.
 *
 * @param head The first vertex id in the sorted array.
 **/
class HorizontalSortedVerticesIds(private val head: String)(implicit factory: GeometryFactory, verticesMap: VerticesMap) {

  /** The action sorted list that contains the sorted vertices ids. */
  val actionSortedList = new ActionSortedList[String](head)

  /** The map that contains the middles for the line of the latest inserted vertex. */
  val middlesMap: MiddlesMap = new MiddlesMap()

  /** A list of intersecting lines that are calculated on the `getLTRIntersectingLines` and `getRTLIntersectingLines` methods. */
  var intersectingLines: List[IntersectingLine] = _

  /** Gets the current index's vertex id. */
  def getCurrentIndexVertexId: String = {
    actionSortedList.getCurrentIndexItem
  }

  /** Prepend intersection's vertex id to `lesserPendingItems`. */
  def addLTRIntersectionVertex(vertex: Vertex): Unit = {
    actionSortedList.addLesserPendingItem(vertex.id)
  }

  /** Prepend intersection's vertex id to `greaterPendingItems`. */
  def addRTLIntersectionVertex(vertex: Vertex): Unit = {
    actionSortedList.addGreaterPendingItem(vertex.id)
  }

  /** Using the `addGreater` sort with action method, for every vertex that passes by during the sort process,
   * finds it's line's intersection point (if there is) with the line of the vertex that we're sorting in.
   *
   * @param leftVertex The last vertex that was sorted in.
   * @param rightVertex The vertex that we're sorting in.
   * @param slope The slope of the line
   * @return List lines of all the vertices that passes by during the sort process,
   * that intersects with the line of the vertex that we're sorting in.
   */
  def getLTRIntersectingLines(leftVertex: Vertex, rightVertex: Vertex, slope: Double): List[IntersectingLine] = {
    intersectingLines = List.empty
    actionSortedList.addGreater(rightVertex.id, getVertexX, getLTRIntersectingLineAction(leftVertex, rightVertex, slope))

    val intersectingMiddles = middlesMap.getLTRIntersections(leftVertex, rightVertex, slope)

    intersectingLines ++ intersectingMiddles
  }

  /** Using the `addLesser` sort with action method, for every vertex that passes by during the sort process,
   * finds it's line's intersection point (if there is) with the line of the vertex that we're sorting in.
   *
   * @param leftVertex The vertex that we're sorting in.
   * @param rightVertex The last vertex that was sorted in.
   * @param slope The slope of the line
   * @return List lines of all the vertices that passes by during the sort process,
   * that intersects with the line of the vertex that we're sorting in.
   */
  def getRTLIntersectingLines(leftVertex: Vertex, rightVertex: Vertex, slope: Double): List[IntersectingLine] = {
    intersectingLines = List.empty
    actionSortedList.addLesser(leftVertex.id, getVertexX, getRTLIntersectingLineAction(leftVertex, rightVertex, slope))

    val intersectingMiddles = middlesMap.getRTLIntersections(leftVertex, rightVertex, slope)

    intersectingLines ++ intersectingMiddles
  }

  /** Get x value of a vertex. */
  private def getVertexX(vertexId: String): Double = {
    verticesMap.get(vertexId).x
  }

  /** Sort action that for every vertex that passes by during the sort process,
   * finds it's line's intersection point (if exists) with the line of the vertex that we're sorting in.
   * If an intersection was found, add it's line to the `intersectingLines` list.
   *
   * @param leftVertex Previous sorted vertex.
   * @param rightVertex Vertex that's currently being sorted in.
   * @param slope Slope of the line of the vertex that's currently being sorted in.
   */
  private def getLTRIntersectingLineAction(leftVertex: Vertex, rightVertex: Vertex, slope: Double): String => Unit = (leftTestVertexId: String) => {
    val leftTestVertex = verticesMap.get(leftTestVertexId)

    leftTestVertex.right.foreach(rightTestNeighbor => {
      val rightTestVertex = verticesMap.get(rightTestNeighbor.id)
      val testSlope = rightTestNeighbor.slope

      if (rightVertex.x > rightTestVertex.x) {
        val leftRangeSlope = Equations.getSlope(leftVertex, leftTestVertex)
        val rightRangeSlope = Equations.getSlope(leftVertex, rightTestVertex)
        if ((rightRangeSlope > testSlope && slope >= rightRangeSlope && slope <= leftRangeSlope) ||
            (rightRangeSlope < testSlope && slope <= rightRangeSlope && slope >= leftRangeSlope) ||
            (rightRangeSlope == leftRangeSlope)) {
          val lineString = factory.createLineString(Array(new Coordinate(leftVertex.x, leftVertex.y), new Coordinate(rightVertex.x, rightVertex.y)))
          val testLineString = factory.createLineString(Array(new Coordinate(leftTestVertex.x, leftTestVertex.y), new Coordinate(rightTestVertex.x, rightTestVertex.y)))
          val intersection = lineString.intersection(testLineString).getCoordinate

          if (intersection != null) {
            if (rightRangeSlope == leftRangeSlope) {
              throw new Exception("Can't handle geometries with overlapping lines.")
            }

            if (rightRangeSlope == slope || leftRangeSlope == slope) {
              throw new Exception("Can't handle geometries with intersections at existing vertex.")
            }

            intersectingLines = intersectingLines :+ IntersectingLine(intersection, leftTestVertex.id, rightTestVertex.id, testSlope)
          }
        }
      } else {
        val middleId = s"${leftTestVertex.id}_${rightTestVertex.id}"
        middlesMap.add(Middle(middleId, leftTestVertex.id, rightTestVertex.id, testSlope))
      }
    })
  }

  /** Sort action that for every vertex that passes by during the sort process,
   * finds it's line's intersection point (if exists) with the line of the vertex that we're sorting in.
   * If an intersection was found, add it's line to the `intersectingLines` list.
   *
   * @param leftVertex Vertex that's currently being sorted in.
   * @param rightVertex Previous sorted vertex.
   * @param slope Slope of the line of the vertex that's currently being sorted in.
   */
  private def getRTLIntersectingLineAction(leftVertex: Vertex, rightVertex: Vertex, slope: Double): String => Unit = (rightTestVertexId: String) => {
    val rightTestVertex = verticesMap.get(rightTestVertexId)

    rightTestVertex.left.foreach(leftTestNeighbor => {
      val leftTestVertex = verticesMap.get(leftTestNeighbor.id)
      val testSlope = leftTestNeighbor.slope

      if (leftVertex.x < leftTestVertex.x) {
        val leftRangeSlope = Equations.getSlope(rightVertex, leftTestVertex)
        val rightRangeSlope = Equations.getSlope(rightVertex, rightTestVertex)
        if ((leftRangeSlope > testSlope && slope >= leftRangeSlope && slope <= rightRangeSlope) ||
            (leftRangeSlope < testSlope && slope <= leftRangeSlope && slope >= rightRangeSlope) ||
            (leftRangeSlope == rightRangeSlope)) {
          val lineString = factory.createLineString(Array(new Coordinate(leftVertex.x, leftVertex.y), new Coordinate(rightVertex.x, rightVertex.y)))
          val testLineString = factory.createLineString(Array(new Coordinate(leftTestVertex.x, leftTestVertex.y), new Coordinate(rightTestVertex.x, rightTestVertex.y)))
          val intersection = lineString.intersection(testLineString).getCoordinate

          if (intersection != null) {
            if (leftRangeSlope == rightRangeSlope) {
              throw new Exception("Can't handle geometries with overlapping lines.")
            }

            if (leftRangeSlope == slope || rightRangeSlope == slope) {
              throw new Exception("Can't handle geometries with intersections at existing vertex.")
            }

            intersectingLines = intersectingLines :+ IntersectingLine(intersection, leftTestVertex.id, rightTestVertex.id, testSlope)
          }
        }
      } else {
        val middleId = s"${leftTestVertex.id}_${rightTestVertex.id}"
        middlesMap.add(Middle(middleId, leftTestVertex.id, rightTestVertex.id, testSlope))
      }
    })
  }
}
