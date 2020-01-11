package kmeans

import scala.annotation.tailrec

object Kmeans {
  def distance(pointA: List[Double], pointB: List[Double]): Double = {
    val sum = pointA.zip(pointB).map { case (a, b) => Math.pow(a - b, 2.0) }.sum
    Math.sqrt(sum)
  }

  def centerOfCluster(points: List[List[Double]]): List[Double] = points.transpose.map(l => l.sum / l.length)
}

case class Kmeans(numberOfCluster: Int) {

  type Point = List[Double]

  def learnFrom[FeaturesRow](indexedRows: Map[Long, FeaturesRow],
                clusterPositionInitializer: Int => (Int, Point)
               ): List[Map[Long, FeaturesRow]] = {
    ???
  }

  def learnFrom(indexedRows: Map[Long, Point],
                clusterPositionInitializer: Int => (Int, Point)
               ): List[Map[Long, Point]] = {
    val clusterIds = Range(1, numberOfCluster + 1)
    val initialClusterPosition: Map[Int, Point] = clusterIds.map(clusterPositionInitializer).toMap
    val stabilizedClusters: Map[Int, Iterable[Long]] = stabilizeClusters(initialClusterPosition, indexedRows)
    stabilizedClusters.map {
      case (_, rowsId) => rowsId.map(id => id -> indexedRows(id)).toMap
    }.toList
  }

  @tailrec
  private def stabilizeClusters(clusterPosition: Map[Int, Point],
                                indexedRows: Map[Long, Point]): Map[Int, Iterable[Long]] = {
    val clusters: Map[Int, Iterable[Long]] = associateToCluster(indexedRows, clusterPosition)

    def computeCenterPosition(clusterId: Int): Point = {
      clusters.get(clusterId)
        .map((cluster: Iterable[Long]) => Kmeans.centerOfCluster(cluster.map(rowId => indexedRows(rowId)).toList))
        .getOrElse(clusterPosition(clusterId))
    }

    val newClusterPosition = clusterPosition.keys.map(id => id -> computeCenterPosition(id)).toMap

    if (clusterPosition == newClusterPosition) {
      clusters
    } else {
      stabilizeClusters(newClusterPosition, indexedRows)
    }
  }

  private def associateToCluster[RowType](indexedRows: Map[Long, Point],
                                          clustersWithPosition: Map[Int, Point]): Map[Int, Iterable[Long]] = {
    def findClosestCluster(point: Point): Int = clustersWithPosition.map {
      case (clusterId, centerPosition) => (clusterId, Kmeans.distance(centerPosition, point))
    }.minBy { case (_, distance) => distance }._1

    indexedRows.map {
      case (rowId, features) => rowId -> findClosestCluster(features)
    }.groupMap(_._2)(_._1)
  }


}
