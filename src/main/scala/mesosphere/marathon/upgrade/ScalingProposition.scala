package mesosphere.marathon.upgrade

import mesosphere.marathon.Protos.MarathonTask

case class ScalingProposition(tasksToKill: Option[Seq[MarathonTask]], tasksToStart: Option[Int])

object ScalingProposition {
  def propose(runningTasks: Set[MarathonTask],
              toKill: Option[Set[MarathonTask]],
              scaleTo: Int): ScalingProposition = {
    val (sentencedAndRunning, notSentencedAndRunning) = runningTasks partition toKill.getOrElse(Set.empty)
    val killCount = math.max(runningTasks.size - scaleTo, sentencedAndRunning.size)
    val ordered = sentencedAndRunning.toSeq ++ notSentencedAndRunning.toSeq.sortBy(_.getStartedAt).reverse
    val candidatesToKill = ordered.take(killCount)
    val numberOfTasksToStart = scaleTo - runningTasks.size + killCount

    val tasksToKill = if (candidatesToKill.nonEmpty) Some(candidatesToKill) else None
    val tasksToStart = if (numberOfTasksToStart > 0) Some(numberOfTasksToStart) else None

    ScalingProposition(tasksToKill, tasksToStart)
  }

}
