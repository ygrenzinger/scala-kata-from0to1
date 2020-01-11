package kmeans

import org.scalatest.FunSuite

class KmeansSuite extends FunSuite {

  test("create one cluster and associate it all points") {
    val kmeans = Kmeans(1)
    val twoPoints = Map(1L -> List(0.0), 2L -> List(10.0))
    val positionInitializer: Int => (Int, List[Double]) = _ => 1 -> List(5.0)
    val result = kmeans.learnFrom(twoPoints, positionInitializer)
    assert(result == List(Map(1L -> List(0.0), 2L -> List(10.0))))
  }

  test("create two cluster and associate it their closest points") {
    val kmeans = Kmeans(2)
    val twoPoints = Map(1L -> List(0.0), 2L -> List(10.0))
    val positionInitializer: Int => (Int, List[Double]) = i => i -> Map(1 -> List(0.0), 2 -> List(10.0))(i)
    val result = kmeans.learnFrom(twoPoints, positionInitializer)
    assert(result == List(Map(1L -> List(0.0)), Map(2L -> List(10.0))))
  }

  test("create clusters by moving centers until stabilization") {
    val kmeans = Kmeans(2)
    val threePoints = Map(1L -> List(0.0), 2L -> List(8.0), 3L -> List(10.0))
    val positionInitializer: Int => (Int, List[Double]) = i => i -> Map(1 -> List(10.0), 2 -> List(11.0))(i)
    val result = kmeans.learnFrom(threePoints, positionInitializer)
    assert(result == List(
      Map(1L -> List(0.0)),
      Map(2L -> List(8.0), 3L -> List(10.0))
    ))
  }

  test("compute distance on one dimension") {
    val a: List[Double] = List(1.0)
    val b: List[Double] = List(3.0)
    assert(Kmeans.distance(a, b) == 2.0)
  }

  test("compute distance on multiples dimensions") {
    val a: List[Double] = List(1.0, 5.0)
    val b: List[Double] = List(5.0, 2.0)
    assert(Kmeans.distance(a, b) == 5.0)
  }

  test("compute center of a cluster") {
    val a: List[Double] = List(0.0, 0.0)
    val b: List[Double] = List(6.0, 10.0)
    assert(Kmeans.centerOfCluster(List(a, b)) == List(3.0, 5.0))
  }

}
