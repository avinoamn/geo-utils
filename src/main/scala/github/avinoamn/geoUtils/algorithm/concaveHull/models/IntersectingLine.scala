package github.avinoamn.geoUtils.algorithm.concaveHull.models

import github.avinoamn.geoUtils.algorithm.concaveHull.types.IntersectingLineTypes.IntersectingLineType
import org.locationtech.jts.geom.Coordinate

/** Representation of a line that's intersecting with the line of the vertex that we want to sort in.
 *
 * @param intersection Coordinate of the intersections.
 * @param left The left vertex of this line.
 * @param right The right vertex of this line.
 * @param slope Slope of this line.
 * @param `type` The type of the intersection line.
 */
case class IntersectingLine(intersection: Coordinate, left: Vertex, right: Vertex, slope: Double,
                            `type`: IntersectingLineType) {

  /** Checks which vertex points to the other using their `next` property,
   * and returns a matching `HeadAndTail` case class. */
  def getHeadAndTail: HeadAndTail = {
    if (left.next == right) {
      HeadAndTail(left, right)
    } else {
      HeadAndTail(right, left)
    }
  }
}
