package github.avinoamn.geoUtils.algorithm.concaveHull.models

/** When at least one of the current iteration line's vertices is found between two neighboring points on the x axis,
 * the line consisting of the two points is called the "middle" line.
 *
 * @param id Concatenation of `left.id` together with `right.id`.
 * @param left The left vertex of this line.
 * @param right The right vertex of this line.
 * @param slope Slope of this line.
 */
case class Middle(id: String, left: Vertex, right: Vertex, slope: Double) extends DirectedLine
