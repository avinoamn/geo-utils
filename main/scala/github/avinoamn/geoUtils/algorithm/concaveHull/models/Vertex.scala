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
  var next: String = _

  /** Lists of the left/right neighbors of the coordinate (2 max total). */
  var left: List[Neighbor] = List.empty
  var right: List[Neighbor] = List.empty

  /** Set's `this` vertex's `next`. */
  def setNext(id: String): Unit = {
    next = id
  }

  /** Add a neighbor to the left of `this` vertex. */
  def addLeft(id: String, slope: Double): Unit = {
    left = left :+ Neighbor(id, slope)
  }

  /** Add a neighbor to the right of `this` vertex. */
  def addRight(id: String, slope: Double): Unit = {
    right = right :+ Neighbor(id, slope)
  }

  /** Replaces a neighbor to the left of `this` vertex (slope stays the same).
   *
   * @param oldId Old neighbor id.
   * @param newId New neighbor id.
   */
  def replaceLeft(oldId: String, newId: String): Unit = {
    left = left.map(neighbor => {
      neighbor.id == oldId match {
        case true => neighbor.copy(id = newId)
        case false => neighbor
      }
    })
  }

  /** Replaces a neighbor to the right of `this` vertex (slope stays the same).
   *
   * @param oldId Old neighbor id.
   * @param newId New neighbor id.
   */
  def replaceRight(oldId: String, newId: String): Unit = {
    right = right.map(neighbor => {
      neighbor.id == oldId match {
        case true => neighbor.copy(id = newId)
        case false => neighbor
      }
    })
  }

  /** Remove a neighbor to the left of `this` vertex.
   *
   * @param id Id of the neighbor to remove.
   */
  def removeLeft(id: String): Unit = {
    left = left.filter(neighbor => neighbor.id != id)
  }

  /** Remove a neighbor to the right of `this` vertex.
   *
   * @param id Id of the neighbor to remove.
   */
  def removeRight(id: String): Unit = {
    right = right.filter(neighbor => neighbor.id != id)
  }

  /** Set vertex as `this` vertex's `next`, and as it's left neighbor.
   *
   * @param next Vertex to set as `this` vertex's `next`.
   * @param slope The slope between `this` vertex and it's next vertex.
   */
  def setLeftNext(next: Vertex, slope: Double): Unit = {
    this.addLeft(next.id, slope)
    next.addRight(this.id, slope)

    this.setNext(next.id)
  }

  /** Set vertex as `this` vertex's `next`, and as it's right neighbor.
   *
   * @param next Vertex to set as `this` vertex's `next`.
   * @param slope The slope between `this` vertex and it's next vertex.
   */
  def setRightNext(next: Vertex, slope: Double): Unit = {
    this.addRight(next.id, slope)
    next.addLeft(this.id, slope)

    this.setNext(next.id)
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
   * @param slope The slope between `this` vertex and it's next vertex.
   * @param isCurrentNextLeft Is the current next vertex to the right of `this` vertex.
   */
  def replaceNext(currNext: Vertex, newNext: Vertex, slope: Double, isCurrentNextLeft: Boolean): Unit = {
    if (isCurrentNextLeft) {
      this.replaceLeft(currNext.id, newNext.id)
      currNext.removeRight(this.id)
      newNext.addRight(this.id, slope)

      this.setNext(newNext.id)
    } else {
      this.replaceRight(currNext.id, newNext.id)
      currNext.removeLeft(this.id)
      newNext.addLeft(this.id, slope)

      this.setNext(newNext.id)
    }
  }
}