package mesosphere.marathon.upgrade

import mesosphere.marathon.Protos.MarathonTask
import org.scalatest.{ FunSuite, Matchers }

class ScalingPropositionTest extends FunSuite with Matchers {

  test("custom apply - tasksToKill: empty should be None") {
    val running = Set.empty[MarathonTask]
    val toKill = Some(Set.empty[MarathonTask])
    val scaleTo = 0
    val proposition = ScalingProposition.propose(running, toKill, scaleTo)
    proposition.tasksToKill shouldBe empty
  }

  test("custom apply - tasksToKill: nonEmpty should be Some") {
    val task = MarathonTask.getDefaultInstance
    val running = Set(task)
    val toKill = Some(Set(task))
    val scaleTo = 0
    val proposition = ScalingProposition.propose(running, toKill, scaleTo)
    proposition.tasksToKill shouldEqual Some(Seq(task))
  }

  test("custom apply - tasksToStart: 0 should be None") {
    val running = Set.empty[MarathonTask]
    val toKill = Some(Set.empty[MarathonTask])
    val scaleTo = 0
    val proposition = ScalingProposition.propose(running, toKill, scaleTo)
    proposition.tasksToStart shouldBe empty
  }

  test("custom apply - tasksToStart: negative number should be None") {
    val running = Set.empty[MarathonTask]
    val toKill = Some(Set.empty[MarathonTask])
    val scaleTo = -42
    val proposition = ScalingProposition.propose(running, toKill, scaleTo)
    proposition.tasksToStart shouldBe empty
  }

  test("custom apply - tasksToStart: positive number should be Some") {
    val running = Set.empty[MarathonTask]
    val toKill = Some(Set.empty[MarathonTask])
    val scaleTo = 42

    val proposition = ScalingProposition.propose(running, toKill, scaleTo)
    proposition.tasksToStart shouldBe Some(42)
  }

}
