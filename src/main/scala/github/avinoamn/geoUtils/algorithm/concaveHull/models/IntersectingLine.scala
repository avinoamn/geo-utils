package github.avinoamn.geoUtils.algorithm.concaveHull.models

import github.avinoamn.geoUtils.algorithm.concaveHull.dataStructures.VerticesMap
import org.locationtech.jts.geom.Coordinate

/** Representation of a line that's intersecting with the line of the vertex that we want to sort in.
 *
 * @param intersection Coordinate of the intersections.
 * @param left Id of the left vertex of this line.
 * @param right Id of the right vertex of this line.
 * @param slope Slope of this line.
 * @param verticesMap Implicit of the verticesMap.
 */
case class IntersectingLine(intersection: Coordinate, left: String, right: String, slope: Double)
                           (implicit verticesMap: VerticesMap) {

  /** Checks which vertex points to the other using their `next` property,
   * and returns a matching `HeadAndTail` case class. */
  def getHeadAndTail: HeadAndTail = {
    if (verticesMap.get(left).next == right) {
      HeadAndTail(left, right)
    } else {
      HeadAndTail(right, left)
    }
  }
}
