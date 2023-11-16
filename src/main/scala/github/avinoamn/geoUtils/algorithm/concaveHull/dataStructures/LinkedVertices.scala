package github.avinoamn.geoUtils.algorithm.concaveHull.dataStructures

import github.avinoamn.geoUtils.algorithm.concaveHull.models.Vertex
import org.locationtech.jts.geom.Coordinate

import scala.annotation.tailrec

/** Vertices in a geometry act like elements of a linked list.
 * This class contains linked list like methods for vertices use.
 * */
class LinkedVertices(val head: Vertex) {

  /** Initialize the linked vertices list from an array of coordinates. */
  def this(coordinates: Array[Coordinate]) = {
    this(new Vertex(coordinates.head.x, coordinates.head.y, isHead = true))

    coordinates.slice(1, coordinates.length).zipWithIndex.foldLeft(head) {
      case (prevVertex, (coordinate, index)) => {
        val newVertex = new Vertex(coordinate.x, coordinate.y, isTail = (index == coordinates.length - 2))
        prevVertex.next = newVertex

        newVertex
      }
    }
  }

  /** Replace a vertex in the list with another. */
  def replace(vertex: Vertex, replacement: Vertex, slope: Double): Unit = {
    val next = vertex.next

    if (vertex.x < next.x) {
      vertex.removeRight(next)
      next.replaceLeft(vertex, replacement)
      replacement.addRight(next, slope)
    } else {
      vertex.removeLeft(next)
      next.replaceRight(vertex, replacement)
      replacement.addLeft(next, slope)
    }

    replacement.setNext(next)
  }

  /** Replace the `next` of a vertex. */
  def replaceNext(vertex: Vertex, replacement: Vertex, slope: Double): Unit = {
    val oldNext = vertex.next

    if (vertex.x < oldNext.x) {
      oldNext.removeLeft(vertex)
      vertex.replaceRight(vertex, replacement)
      replacement.addLeft(vertex, slope)
    } else {
      oldNext.removeRight(vertex)
      vertex.replaceLeft(vertex, replacement)
      replacement.addRight(vertex, slope)
    }

    vertex.setNext(replacement)
  }

  /** Set two vertices as neighbors and trailing. */
  def setNext(vertex: Vertex, next: Vertex, slope: Double): Unit = {
    setNeighbors(vertex, next, slope)

    vertex.setNext(next)
  }

  /** Set two vertices as neighbors. */
  def setNeighbors(vertex1: Vertex, vertex2: Vertex, slope: Double): Unit = {
    if (vertex1.x < vertex2.x) {
      vertex1.addRight(vertex2, slope)
      vertex2.addLeft(vertex1, slope)
    } else {
      vertex1.addLeft(vertex2, slope)
      vertex2.addRight(vertex1, slope)
    }
  }

  /** Reverse the vertices' order in the geometry.
   *
   * @param from The vertex to reverse from.
   * @param to   The vertex to reverse to.
   * */
  def reverse(from: Vertex, to: Vertex): Unit = {
    @tailrec
    def reverse(prev: Vertex = null, curr: Vertex = from): Unit = {
      if (prev != to) {
        val next = curr.next
        curr.next = prev

        reverse(curr, next)
      }
    }

    reverse()
  }

  /** Get list of vertices ordered by their order in the geometry. */
  def getOrdered: List[Vertex] = {
    @tailrec
    def getOrdered(list: List[Vertex] = List(), current: Vertex = head): List[Vertex] = {
      if (current == null) {
        list
      } else {
        getOrdered(list :+ current, current.next)
      }
    }

    getOrdered()
  }

  /** Iterates over the linked list of vertices by pairs, and run a callback against each pair. */
  def fold(callback: (Vertex, Vertex) => Unit): Unit = {
    @tailrec
    def fold(prevVertex: Vertex, vertex: Vertex): Unit = {
      if (vertex != null) {
        callback(prevVertex, vertex)

        fold(vertex, vertex.next)
      }
    }

    fold(head, head.next)
  }
}
