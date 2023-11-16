package github.avinoamn.geoUtils.algorithm.concaveHull.models

/** Representation of a line by the direction of it's vertices. */
trait DirectedLine {

  /** The left vertex of this line. */
  def left: Vertex

  /** The right vertex of this line. */
  def right: Vertex

  /** The slope of this line. */
  def slope: Double

  /** Checks which vertex points to the other using their `next` property,
   * and returns a matching head and tail tuple. */
  lazy val (head, tail): (Vertex, Vertex) = if (left.next == right) (left, right) else (right, left)
}
