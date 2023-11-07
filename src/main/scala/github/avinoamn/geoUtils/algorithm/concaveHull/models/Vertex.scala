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

  /** Lists of the left/right neighbors of the coordinate (2 max total). */
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
    this.left = this.left.map(neighbor => {
      if (neighbor.vertex.id == oldVertex.id) {
        oldVertex.removeRight(this.id)
        newVertex.addRight(this, neighbor.slope)
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
    this.right = this.right.map(neighbor => {
      if (neighbor.vertex.id == oldVertex.id) {
        oldVertex.removeLeft(this.id)
        newVertex.addLeft(this, neighbor.slope)
        neighbor.copy(vertex = newVertex)
      } else {
        neighbor
      }
    })
  }

  /** Remove a neighbor to the left of `this` vertex.
   *
   * @param id Id of the neighbor to remove.
   */
  def removeLeft(id: String): Unit = {
    left = left.filter(neighbor => neighbor.vertex.id != id)
  }

  /** Remove a neighbor to the right of `this` vertex.
   *
   * @param id Id of the neighbor to remove.
   */
  def removeRight(id: String): Unit = {
    right = right.filter(neighbor => neighbor.vertex.id != id)
  }

  /** Set vertex as `this` vertex's `next`, and as it's left neighbor.
   *
   * @param next Vertex to set as `this` vertex's `next`.
   * @param slope The slope between `this` vertex and it's `next` vertex.
   */
  def setLeftNext(next: Vertex, slope: Double): Unit = {
    this.addLeft(next, slope)
    next.addRight(this, slope)

    this.setNext(next)
  }

  /** Set vertex as `this` vertex's `next`, and as it's right neighbor.
   *
   * @param next Vertex to set as `this` vertex's `next`.
   * @param slope The slope between `this` vertex and it's `next` vertex.
   */
  def setRightNext(next: Vertex, slope: Double): Unit = {
    this.addRight(next, slope)
    next.addLeft(this, slope)

    this.setNext(next)
  }

  /** Set vertex as `this` vertex's `next`, and as one of its neighbors.
   *
   * @param next Vertex to set as `this` vertex's `next`.
   * @param slope The slope between `this` vertex and it's next vertex.
   * @param isNextRight Is the new next vertex to the right of `this` vertex.
   */
  def setNext(next: Vertex, slope: Double, isNextRight: Boolean): Unit = {
    if (isNextRight) {
      setRightNext(next, slope)
    } else {
      setLeftNext(next, slope)
    }
  }

  /** Replace the current `next` vertex of `this` vertex with a new one.
   *
   * @param currNext Current `next` of `this` vertex.
   * @param newNext New vertex to set as `this` vertex's `next`.
   * @param isCurrentNextLeft Is the current next vertex to the right of `this` vertex.
   */
  def replaceNext(currNext: Vertex, newNext: Vertex, isCurrentNextLeft: Boolean): Unit = {
    if (isCurrentNextLeft) {
      this.replaceLeft(currNext, newNext)

      this.setNext(newNext)
    } else {
      this.replaceRight(currNext, newNext)

      this.setNext(newNext)
    }
  }
}