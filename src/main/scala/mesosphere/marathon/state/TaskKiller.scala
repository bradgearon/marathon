package mesosphere.marathon.state

import javax.inject.Inject

import mesosphere.marathon.MarathonSchedulerService
import mesosphere.marathon.Protos.MarathonTask
import mesosphere.marathon.state.TaskKiller.{ AppNotFound, GroupNotFound, KillRequested, KillResponse }
import mesosphere.marathon.tasks.TaskTracker

import scala.concurrent.Future

class TaskKiller @Inject() (
    taskTracker: TaskTracker,
    groupManager: GroupManager,
    service: MarathonSchedulerService) {

  def kill(pathId: PathId,
           findToKill: (Set[MarathonTask] => Set[MarathonTask]),
           scale: Boolean): Future[KillResponse] = {
    import mesosphere.util.ThreadPoolContext.context

    if (taskTracker.contains(pathId)) {
      val tasks = taskTracker.get(pathId)
      val toKill = findToKill(tasks)
      if (scale) {
        groupManager.group(pathId.parent).flatMap {
          case Some(group) =>
            def updateAppFunc(current: AppDefinition) = current.copy(instances = current.instances - toKill.size)
            val deploymentPlan = groupManager.updateApp(
              pathId, updateAppFunc, Timestamp.now(), force = false, toKill = toKill)
            deploymentPlan.map(_ => KillRequested(toKill))

          case None => Future.successful(GroupNotFound(pathId.parent))
        }
      }
      else {
        service.killTasks(pathId, toKill)
        Future.successful(KillRequested(toKill))
      }
    }
    else {
      Future.successful(AppNotFound(pathId))
    }
  }

}

object TaskKiller {
  sealed trait KillResponse
  case class AppNotFound(appId: PathId) extends KillResponse
  case class GroupNotFound(groupId: PathId) extends KillResponse
  case class KillRequested(tasks: Set[MarathonTask]) extends KillResponse
}
