package github.avinoamn.geoUtils.algorithm.concaveHull.models

/** Representation of a line by it's start and end. */
case class Line(head: Vertex, tail: Vertex, slope: Double) {

  /** Is the line's direction left-to-right. */
  lazy val isLTR: Boolean = head.x < tail.x
}
