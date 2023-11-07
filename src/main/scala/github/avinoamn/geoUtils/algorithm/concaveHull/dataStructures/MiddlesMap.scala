package github.avinoamn.geoUtils.algorithm.concaveHull.dataStructures

import github.avinoamn.geoUtils.algorithm.concaveHull.models.{IntersectingLine, Middle, Vertex}
import github.avinoamn.geoUtils.algorithm.concaveHull.types.IntersectingLineTypes
import github.avinoamn.geoUtils.algorithm.concaveHull.utils.math.Equations
import org.locationtech.jts.geom.{Coordinate, GeometryFactory}

/** Manages a map of middle lines.
 * The keys are the middles' ids, and the values are the middles themselves. */
class MiddlesMap(implicit factory: GeometryFactory) {

  /** The map that contains the vertices. */
  var map: Map[String, Middle] = Map()

  /** Adds a middle to the map. */
  def add(middle: Middle): Unit = {
    map = map + (middle.id -> middle)
  }

  /** Removes a middle from the map by it's id. */
  def remove(id: String): Unit = {
    map = map - id
  }

  /** Gets a list of all the middles in the map. */
  def getAll: List[Middle] = {
    map.values.toList
  }

  /** Gets intersection lines from all the middles that intersects with a line that comes from left to right.
   *
   * @param leftVertex Left vertex of the line that we check for intersections with.
   * @param rightVertex Right vertex of the line that we check for intersections with.
   * @param slope Slope of the line that we check for intersections with.
   * @return List of intersecting lines.
   */
  def getLTRIntersections(leftVertex: Vertex, rightVertex: Vertex, slope: Double): List[IntersectingLine] = {
    var intersectingMiddles: List[IntersectingLine] = List.empty

    getAll.foreach(middle => {
      val middleLeftVertex = middle.left
      val middleRightVertex = middle.right
      val middleSlope = middle.slope

      val leftRangeSlope = Equations.getSlope(leftVertex, middleLeftVertex)
      val rightRangeSlope = Equations.getSlope(leftVertex, middleRightVertex)

      /** Using slopes, checks if the line of the vertex that we want to sort in
       * might intersect with the current iteration middle. */
      def mightIntersect: Boolean = {
        /** left -> middle */
        if (leftVertex.x < middleLeftVertex.x && rightVertex.x <= middleRightVertex.x) {
          if ((rightRangeSlope > middleSlope && slope >= rightRangeSlope && slope <= leftRangeSlope) ||
              (rightRangeSlope < middleSlope && slope <= rightRangeSlope && slope >= leftRangeSlope) ||
              (slope == middleSlope)) {
            return true
          }
          return false
        }

        /** middle -> right */
        if (middleLeftVertex.x <= leftVertex.x && middleRightVertex.x < rightVertex.x) {
          /** Remove this middle from the map since the vertex that we want to sort in is going out of this middle */
          remove(middle.id)

          if ((rightRangeSlope > middleSlope && slope >= rightRangeSlope) ||
              (rightRangeSlope < middleSlope && slope <= rightRangeSlope)) {
            return true
          }
          return false
        }

        /** middle -> middle */
        if (middleLeftVertex.x <= leftVertex.x && rightVertex.x <= middleRightVertex.x) {
          if ((rightRangeSlope > middleSlope && slope >= rightRangeSlope) ||
              (rightRangeSlope < middleSlope && slope <= rightRangeSlope)) {
            return true
          }
          return false
        }

        /** Throw error if non of the condition matched (it means that the middle is not really a middle).
         * (Shouldn't happen. If it happens then the code that added it to the map is wrong.) */
        throw new Error("Invalid Middle error.")
      }

      if (mightIntersect) {
        val lineString = factory.createLineString(Array(new Coordinate(leftVertex.x, leftVertex.y), new Coordinate(rightVertex.x, rightVertex.y)))
        val middleLineString = factory.createLineString(Array(new Coordinate(middleLeftVertex.x, middleLeftVertex.y), new Coordinate(middleRightVertex.x, middleRightVertex.y)))
        val intersection = lineString.intersection(middleLineString).getCoordinate

        if (intersection != null) {
          if (slope == middleSlope) {
            throw new Exception("Can't handle geometries with overlapping lines.")
          }

          if (Equations.isCoordinateOnLine(lineString.getCoordinates.last, middleLineString)) {
            if (!rightVertex.isTail) {
              throw new Exception("Can't handle geometries with intersections at existing vertex.")
            }
          } else {
            if (leftRangeSlope == slope || rightRangeSlope == slope) {
              throw new Exception("Can't handle geometries with intersections at existing vertex.")
            }

            val intersectingLine = IntersectingLine(intersection, middleLeftVertex, middleRightVertex, middleSlope, IntersectingLineTypes.Middle)
            intersectingMiddles = intersectingMiddles :+ intersectingLine
          }
        }
      }
    })

    intersectingMiddles
  }

  /** Gets intersection lines from all the middles that intersects with a line that comes from right to left.
   *
   * @param leftVertex Left vertex of the line that we check for intersections with.
   * @param rightVertex Right vertex of the line that we check for intersections with.
   * @param slope Slope of the line that we check for intersections with.
   * @return List of intersecting lines.
   */
  def getRTLIntersections(leftVertex: Vertex, rightVertex: Vertex, slope: Double): List[IntersectingLine] = {
    var intersectingMiddles: List[IntersectingLine] = List.empty

    getAll.foreach(middle => {
      val middleLeftVertex = middle.left
      val middleRightVertex = middle.right
      val middleSlope = middle.slope

      val leftRangeSlope = Equations.getSlope(rightVertex, middleLeftVertex)
      val rightRangeSlope = Equations.getSlope(rightVertex, middleRightVertex)

      /** Using slopes, checks if the line of the vertex that we want to sort in
       * might intersect with the current iteration middle. */
      def mightIntersect: Boolean = {
        /** middle <- right */
        if (middleLeftVertex.x <= leftVertex.x &&  middleRightVertex.x < rightVertex.x) {
          if ((leftRangeSlope > middleSlope && slope >= leftRangeSlope && slope <= rightRangeSlope) ||
              (leftRangeSlope < middleSlope && slope <= leftRangeSlope && slope >= rightRangeSlope) ||
              (slope == middleSlope)) {
            return true
          }
          return false
        }

        /** left <- middle */
        if (leftVertex.x < middleLeftVertex.x && rightVertex.x <= middleRightVertex.x) {
          /** Remove this middle from the map since the vertex that we want to sort in is going out of this middle */
          remove(middle.id)

          if ((leftRangeSlope > middleSlope && slope >= leftRangeSlope) ||
              (leftRangeSlope < middleSlope && slope <= leftRangeSlope)) {
            return true
          }
          return false
        }

        /** middle <- middle */
        if (middleLeftVertex.x <= leftVertex.x && rightVertex.x <= middleRightVertex.x) {
          if ((leftRangeSlope > middleSlope && slope >= leftRangeSlope) ||
              (leftRangeSlope < middleSlope && slope <= leftRangeSlope)) {
            return true
          }
          return false
        }

        /** Throw error if non of the condition matched (it means that the middle is not really a middle).
         * (Shouldn't happen. If it happens then the code that added it to the map is wrong.) */
        throw new Error("Invalid Middle error.")
      }

      if (mightIntersect) {
        val lineString = factory.createLineString(Array(new Coordinate(leftVertex.x, leftVertex.y), new Coordinate(rightVertex.x, rightVertex.y)))
        val middleLineString = factory.createLineString(Array(new Coordinate(middleLeftVertex.x, middleLeftVertex.y), new Coordinate(middleRightVertex.x, middleRightVertex.y)))
        val intersection = lineString.intersection(middleLineString).getCoordinate

        if (intersection != null) {
          if (slope == middleSlope) {
            throw new Exception("Can't handle geometries with overlapping lines.")
          }

          if (Equations.isCoordinateOnLine(lineString.getCoordinates.head, middleLineString)) {
            if (!leftVertex.isTail) {
              throw new Exception("Can't handle geometries with intersections at existing vertex.")
            }
          } else {
            if (leftRangeSlope == slope || rightRangeSlope == slope) {
              throw new Exception("Can't handle geometries with intersections at existing vertex.")
            }

            val intersectingLine = IntersectingLine(intersection, middleLeftVertex, middleRightVertex, middleSlope, IntersectingLineTypes.Middle)
            intersectingMiddles = intersectingMiddles :+ intersectingLine
          }
        }
      }
    })

    intersectingMiddles
  }
}
