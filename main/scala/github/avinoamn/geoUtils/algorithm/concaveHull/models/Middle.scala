package github.avinoamn.geoUtils.algorithm.concaveHull.models

/** When the line of the vertex that we want to sort in is found between two neighboring points on the x axis,
 * the line consisting of the two points is called the "middle" line.
 *
 * @param id Concatenation of left together with right.
 * @param left Id of the left vertex of this line.
 * @param right Id of the right vertex of this line.
 * @param slope Slope of this line.
 */
case class Middle(id: String, left: String, right: String, slope: Double)
