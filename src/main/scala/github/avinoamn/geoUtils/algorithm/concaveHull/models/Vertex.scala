package github.avinoamn.geoUtils.algorithm.concaveHull.models

import java.util.UUID

/** Extended representation of a coordinate in a geometry.
 *
 * @param x x value of the coordinate.
 * @param y y value of the coordinate.
 * @param isHead is it the geometry's first coordinate.
 * @param isTail is it the geometry's last coordinate.
 */
class Vertex(val x: Double, val y: Double, val isHead: Boolean = false, val isTail: Boolean = false) {
  /** Unique id. */
  val id: String = UUID.randomUUID().toString

  /** Id of the following coordinate in the geometry. */
  var next: Vertex = _

  /** Lists of the left/right neighbors of the coordinate (2 neighbors max total). */
  var left: List[Neighbor] = List.empty
  var right: List[Neighbor] = List.empty

  /** Set's `this` vertex's `next`. */
  def setNext(vertex: Vertex): Unit = {
    next = vertex
  }

  /** Add a neighbor to the left of `this` vertex. */
  def addLeft(vertex: Vertex, slope: Double): Unit = {
    left = left :+ Neighbor(vertex, slope)
  }

  /** Add a neighbor to the right of `this` vertex. */
  def addRight(vertex: Vertex, slope: Double): Unit = {
    right = right :+ Neighbor(vertex, slope)
  }

  /** Replaces a neighbor to the left of `this` vertex (slope stays the same).
   *
   * @param oldVertex Old neighbor vertex.
   * @param newVertex New neighbor vertex.
   */
  def replaceLeft(oldVertex: Vertex, newVertex: Vertex): Unit = {
    left = left.map(neighbor => {
      if (neighbor.vertex.id == oldVertex.id) {
        neighbor.copy(vertex = newVertex)
      } else {
        neighbor
      }
    })
  }

  /** Replaces a neighbor to the right of `this` vertex (slope stays the same).
   *
   * @param oldVertex Old neighbor vertex.
   * @param newVertex New neighbor vertex.
   */
  def replaceRight(oldVertex: Vertex, newVertex: Vertex): Unit = {
    right = right.map(neighbor => {
      if (neighbor.vertex.id == oldVertex.id) {
        neighbor.copy(vertex = newVertex)
      } else {
        neighbor
      }
    })
  }

  /** Remove a neighbor to the left of `this` vertex.
   *
   * @param vertex Neighboring vertex to remove.
   */
  def removeLeft(vertex: Vertex): Unit = {
    left = left.filter(neighbor => neighbor.vertex == vertex)
  }

  /** Remove a neighbor to the right of `this` vertex.
   *
   * @param vertex Neighboring vertex to remove.
   */
  def removeRight(vertex: Vertex): Unit = {
    right = right.filter(neighbor => neighbor.vertex == vertex)
  }
}