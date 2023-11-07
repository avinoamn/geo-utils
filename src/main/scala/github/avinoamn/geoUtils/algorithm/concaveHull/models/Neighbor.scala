package github.avinoamn.geoUtils.algorithm.concaveHull.models

/** Represents the neighboring vertex of another vertex.
 *
 * @param vertex The neighboring vertex.
 * @param slope Slope between the vertex and it's neighbor.
 * */
case class Neighbor(vertex: Vertex, slope: Double)
