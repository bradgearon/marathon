package mesosphere.marathon.api.v2

import javax.inject.Inject
import javax.ws.rs._
import javax.ws.rs.core.{ MediaType, Response }

import com.codahale.metrics.annotation.Timed
import org.apache.log4j.Logger

import mesosphere.marathon.api.v2.json.EnrichedTask
import mesosphere.marathon.api.{ EndpointsHelper, RestResource }
import mesosphere.marathon.health.HealthCheckManager
import mesosphere.marathon.state.PathId._
import mesosphere.marathon.state.{ TaskKiller, GroupManager, PathId }
import mesosphere.marathon.tasks.TaskTracker
import mesosphere.marathon.{ MarathonConf, MarathonSchedulerService }
import mesosphere.marathon.Protos.MarathonTask

@Produces(Array(MediaType.APPLICATION_JSON))
@Consumes(Array(MediaType.APPLICATION_JSON))
class AppTasksResource @Inject() (service: MarathonSchedulerService,
                                  taskTracker: TaskTracker,
                                  taskKiller: TaskKiller,
                                  healthCheckManager: HealthCheckManager,
                                  val config: MarathonConf,
                                  groupManager: GroupManager) extends RestResource {

  val log = Logger.getLogger(getClass.getName)
  val GroupTasks = """^((?:.+/)|)\*$""".r

  @GET
  @Produces(Array(MediaType.APPLICATION_JSON))
  @Timed
  def indexJson(@PathParam("appId") appId: String): Response = {

    def tasks(appIds: Set[PathId]): Set[EnrichedTask] = for {
      id <- appIds
      health = result(healthCheckManager.statuses(id))
      task <- taskTracker.get(id)
    } yield EnrichedTask(id, task, health.getOrElse(task.getId, Nil))

    val matchingApps = appId match {
      case GroupTasks(gid) =>
        result(groupManager.group(gid.toRootPath))
          .map(_.transitiveApps.map(_.id))
          .getOrElse(Set.empty)
      case _ => Set(appId.toRootPath)
    }

    val running = matchingApps.filter(taskTracker.contains)

    if (running.isEmpty) unknownApp(appId.toRootPath) else ok(Map("tasks" -> tasks(running)))
  }

  @GET
  @Produces(Array(MediaType.TEXT_PLAIN))
  @Timed
  def indexTxt(@PathParam("appId") appId: String): Response = {
    val id = appId.toRootPath
    service.getApp(id).fold(unknownApp(id)) { app =>
      ok(EndpointsHelper.appsToEndpointString(taskTracker, Seq(app), "\t"))
    }
  }

  @DELETE
  @Timed
  def deleteMany(@PathParam("appId") appId: String,
                 @QueryParam("host") host: String,
                 @QueryParam("scale") scale: Boolean = false): Response = {
    val pathId = appId.toRootPath
    def findToKill(appTasks: Set[MarathonTask]): Set[MarathonTask] = Option(host).fold(appTasks) { hostname =>
      appTasks.filter(_.getHost == hostname || hostname == "*")
    }
    def responseForTasks(killRequested: TaskKiller.KillRequested): Response = ok(Map("tasks" -> killRequested.tasks))

    import scala.concurrent.ExecutionContext.Implicits.global
    result(taskKiller.kill(pathId, findToKill, scale).map(responseForKill(responseForTasks)))
  }

  @DELETE
  @Path("{taskId}")
  @Timed
  def deleteOne(@PathParam("appId") appId: String,
                @PathParam("taskId") id: String,
                @QueryParam("scale") scale: Boolean = false): Response = {
    val pathId = appId.toRootPath
    def findToKill(appTasks: Set[MarathonTask]): Set[MarathonTask] = appTasks.find(_.getId == id).toSet

    def responseForTasks(killRequested: TaskKiller.KillRequested): Response =
      killRequested.tasks.headOption.fold(unknownTask(id))(task => ok(Map("task" -> task)))

    import scala.concurrent.ExecutionContext.Implicits.global
    result(taskKiller.kill(pathId, findToKill, scale).map(responseForKill(responseForTasks)))
  }

}
