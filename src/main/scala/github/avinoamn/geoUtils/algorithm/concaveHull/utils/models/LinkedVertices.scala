package github.avinoamn.geoUtils.algorithm.concaveHull.utils.models

import github.avinoamn.geoUtils.algorithm.concaveHull.models.Vertex

import scala.annotation.tailrec

/** Vertices in a geometry act like elements of a linked list.
 * This object contains linked list like methods for vertices use.
 **/
object LinkedVertices {
  /** Reverse the vertices' order in the geometry.
   *
   * @param from The vertex to reverse from.
   * @param to The vertex to reverse to.
   * */
  def reverse(from: Vertex, to: Vertex): Unit = {
    var curr: Vertex = from
    var prev: Vertex = null
    var next: Vertex = null

    while (prev != to) {
      next = curr.next
      curr.next = prev
      prev = curr
      curr = next
    }
  }

  /** Get list of vertices ordered by their order in the geometry. */
  def getOrdered(from: Vertex): List[Vertex] = {
    @tailrec
    def getOrdered(list: List[Vertex] = List(), current: Vertex = from): List[Vertex] = {
      if (current == null) {
        list
      } else {
        getOrdered(list :+ current, current.next)
      }
    }

    getOrdered()
  }
}
