package kmeans

import scala.annotation.tailrec

object Kmeans {
  def distance(pointA: List[Double], pointB: List[Double]): Double = {
    val sum = pointA.zip(pointB).map { case (a, b) => Math.pow(a - b, 2.0) }.sum
    Math.sqrt(sum)
  }

  def centerOfCluster(points: List[Iterable[Double]]): List[Double] = points.transpose.map(l => l.sum / l.length)
}

case class Kmeans(numberOfCluster: Int) {

  def learnFrom[RowType](rows: List[RowType],
                         featuresTransformer: RowType => List[Double],
                         positionInitializer: Int => (Int, List[Double])
                        ): List[List[RowType]] = {
    val clusterIds = Range(1, numberOfCluster + 1)
    val indexedRows = rows.indices.zip(rows).toMap
    val transformedRows: Map[Int, List[Double]] = indexedRows.view.mapValues(featuresTransformer.apply).toMap

    val initialClusterPosition: Map[Int, List[Double]] = clusterIds.map(positionInitializer).toMap
    stabilizeClusters(initialClusterPosition, transformedRows).map {
      case (_, rowsId) => rowsId.map(id => indexedRows(id)).toList
    }.toList
  }

  @tailrec
  private def stabilizeClusters(clusterPosition: Map[Int, List[Double]], rows: Map[Int, List[Double]]): Map[Int, Iterable[Int]] = {
    val clusters: Map[Int, Iterable[Int]] = associateToCluster(rows, clusterPosition)

    def computeCenterPosition(clusterId: Int): List[Double] = {
      clusters.get(clusterId)
        .map((cluster: Iterable[Int]) => Kmeans.centerOfCluster(cluster.map(rowId => rows(rowId)).toList))
        .getOrElse(clusterPosition(clusterId))
    }

    val newClusterPosition = clusterPosition.keys.map(id => id -> computeCenterPosition(id)).toMap

    if (clusterPosition == newClusterPosition) {
      clusters
    } else {
      stabilizeClusters(newClusterPosition, rows)
    }
  }

  private def associateToCluster[RowType](transformedRows: Map[Int, List[Double]],
                                          clustersWithPosition: Map[Int, List[Double]]): Map[Int, Iterable[Int]] = {
    def findClosestCluster(features: List[Double]): Int = clustersWithPosition.map {
      case (clusterId, centerPosition) => (clusterId, Kmeans.distance(centerPosition, features))
    }.minBy { case (_, distance) => distance }._1

    transformedRows.map {
      case (rowId, features) => rowId -> findClosestCluster(features)
    }.groupMap(_._2)(_._1)
  }


}
