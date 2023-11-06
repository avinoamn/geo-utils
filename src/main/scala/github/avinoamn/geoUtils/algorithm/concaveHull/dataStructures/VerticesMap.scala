package github.avinoamn.geoUtils.algorithm.concaveHull.dataStructures

import github.avinoamn.geoUtils.algorithm.concaveHull.models.Vertex

import scala.annotation.tailrec

/** Manages a map of vertices in a geometry.
 * The keys are the vertices' ids, and the values are the vertices themselves.
 *
 * @param head A vertex that represents the first coordinate in the geometry.
 **/
class VerticesMap(private val head: Vertex) {

  /** The map that contains the vertices. */
  var map: Map[String, Vertex] = Map(head.id -> head)

  /** Adds a vertex to the map. */
  def add(vertex: Vertex): Unit = {
    map = map + (vertex.id -> vertex)
  }

  /** Gets a vertex from the map by it's id. */
  def get(id: String): Vertex = {
    map.getOrElse(id, null)
  }

  /** Gets the head vertex of the geometry. */
  def getHead: Vertex = {
    head
  }

  /** Reverse the vertices' order in the geometry.
   *
   * @param from Id of the vertex to reverse from.
   * @param to Id of the vertex to reverse to.
   **/
  def reverse(from: String, to: String): Unit = {
    var curr: Vertex = get(from)
    var prev: String = null
    var next: String = null

    while (prev != to) {
      next = curr.next
      curr.next = prev
      prev = curr.id
      curr = get(next)
    }
  }

  /** Get list of vertices ordered by their order in the geometry. */
  def getOrdered: List[Vertex] = {
    @tailrec
    def getOrdered(list: List[Vertex] = List(), current: String = head.id): List[Vertex] = {
      if (current == null) {
        list
      } else {
        getOrdered(list :+ get(current), get(current).next)
      }
    }
    getOrdered()
  }
}
