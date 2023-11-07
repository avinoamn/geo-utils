package github.avinoamn.geoUtils.algorithm.concaveHull.types

/** Enumerable of intersecting line types.
 *
 * Crossing - When the intersecting line is being crossed from end to end on the (x axis) by the other line.
 *
 * Middle - When the intersecting line is being crossed on the (x axis) by the other line on:
 *  1. one end only.
 *  2. between both ends.
 */
object IntersectingLineTypes extends Enumeration {
  type IntersectingLineType = Value

  val Crossing, Middle = Value
}
