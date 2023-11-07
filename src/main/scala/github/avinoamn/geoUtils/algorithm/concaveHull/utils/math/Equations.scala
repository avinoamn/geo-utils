package github.avinoamn.geoUtils.algorithm.concaveHull.utils.math

import github.avinoamn.geoUtils.algorithm.concaveHull.models.Vertex
import github.avinoamn.geoUtils.algorithm.concaveHull.utils.consts.Distances
import org.locationtech.jts.geom.{Coordinate, LineString}

object Equations {
  def getSlope(head: Vertex, tail: Vertex): Double = {
    if (head.x == tail.x) {
      (head.y - tail.y)/(-1 * Distances.MIN_BUFFER)
    } else {
      (head.y - tail.y)/(head.x - tail.x)
    }
  }

  def isCoordinateOnLine(coordinate: Coordinate, line: LineString): Boolean = {
    val head = line.getCoordinateN(0)
    val tail = line.getCoordinateN(1)

    head.distance(coordinate) + coordinate.distance(tail) == line.getLength
  }
}
