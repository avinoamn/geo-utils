package github.avinoamn.geoUtils.algorithm.concaveHull.models

/** Represents the neighboring vertex of another vertex.
 *
 * @param id Id of the neighboring vertex.
 * @param slope Slope between the vertex and it's neighbor.
 * */
case class Neighbor(id: String, slope: Double)
