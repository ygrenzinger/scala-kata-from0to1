import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers}

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class BuildTaskRunnerSuite extends FunSuite with Matchers with ScalaFutures with BeforeAndAfterEach {

  test("Should execute with success a task") {
    val taskA = BuildTaskRunner.createTask(name = "A", f = () => {})
    whenReady(BuildTaskRunner.toFuture(Seq(taskA))) { result: Seq[Try[String]] =>
      result shouldBe Seq(Success("A"))
    }
  }

  test("Should handle exception in a task") {
    val exception = new IllegalStateException("oops")
    val taskA = BuildTaskRunner.createTask(name = "A", f = () => {
      throw exception
    })

    whenReady(BuildTaskRunner.toFuture(Seq(taskA))) { result: Seq[Try[String]] =>
      result shouldBe Seq(Failure(exception))
    }
  }

  test("Should execute task only once") {
    val runnedTasks = ListBuffer.empty[String]
    val taskA = BuildTaskRunner.createTask(name = "A", f = () => runnedTasks.addOne("A"))

    whenReady(BuildTaskRunner.toFuture(Seq(taskA, taskA))) { result: Seq[Try[String]] =>
      result shouldBe Seq(Success("A"), Success("A"))
      runnedTasks should have size 1
      runnedTasks should contain("A")
    }
  }

  test("Should execute tasks after other tasks which they depends") {
    val runnedTasks = ListBuffer.empty[String]
    val tasks = createComplexGraph(runnedTasks)
    whenReady(BuildTaskRunner.toFuture(tasks)) { result: Seq[Try[String]] =>
      result shouldBe Seq(Success("A"), Success("B"), Success("C"), Success("D"))
      runnedTasks should contain inOrder("C", "D", "A", "B")
    }
  }

  test("Should filter task to execute") {
    val runnedTasks = ListBuffer.empty[String]
    val tasks = createComplexGraph(runnedTasks)
    whenReady(BuildTaskRunner.toFuture(tasks, _.name == "D")) { result: Seq[Try[String]] =>
      result shouldBe Seq(Success("D"))
      runnedTasks should contain inOrder("C", "D")
    }
  }

  private def createComplexGraph(runnedTasks: ListBuffer[String]) = {
    val taskA = BuildTaskRunner.createTask(name = "A", f = () => {
      Thread.sleep(50)
      runnedTasks.addOne("A")
    })
    val taskB = BuildTaskRunner.createTask(name = "B", f = () => {
      runnedTasks.addOne("B")
    }, depends = Seq(taskA))
    val taskC = BuildTaskRunner.createTask(name = "C", f = () => {
      runnedTasks.addOne("C")
    })
    val taskD = BuildTaskRunner.createTask(name = "D", f = () => {
      Thread.sleep(25)
      runnedTasks.addOne("D")
    }, depends = Seq(taskC))

    val tasks = Seq(taskA, taskB, taskC, taskD)
    tasks
  }
}


class BuildTask(val name: String, val f: () => Unit, val depends: Seq[BuildTask]) {
  private lazy val currentTask = Future[Try[String]] {
    Try.apply {
      f()
      name
    }
  }

  lazy val future: Future[Try[String]] = {
    if (depends.isEmpty) {
      currentTask
    } else {
      Future
        .sequence(depends.map(_.future))
        .flatMap(_ => currentTask)
    }
  }

}

object BuildTaskRunner {
  def createTask(name: String,
                 f: () => Unit,
                 depends: Seq[BuildTask] = Seq()): BuildTask =
    new BuildTask(name, f, depends)

  def toFuture(tasks: Seq[BuildTask], predicate: BuildTask => Boolean = _ => true): Future[Seq[Try[String]]] =
    Future.sequence(tasks.filter(predicate).map(_.future))

}
