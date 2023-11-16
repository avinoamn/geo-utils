package github.avinoamn.geoUtils.algorithm.concaveHull.models

import github.avinoamn.geoUtils.algorithm.concaveHull.types.IntersectingLineTypes.IntersectingLineType
import org.locationtech.jts.geom.Coordinate

/** Representation of a line that's intersecting with the current iteration line.
 *
 * @param left The left vertex of this line.
 * @param right The right vertex of this line.
 * @param slope Slope of this line.
 * @param intersection Coordinate of the intersection.
 * @param `type` The type of the intersection line.
 */
case class IntersectingLine(left: Vertex,
                            right: Vertex,
                            slope: Double,
                            intersection: Coordinate,
                            `type`: IntersectingLineType) extends DirectedLine
