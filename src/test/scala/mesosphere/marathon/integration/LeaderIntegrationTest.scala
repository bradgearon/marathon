package mesosphere.marathon.integration

import mesosphere.marathon.integration.setup._
import org.scalatest.concurrent.Eventually
import org.scalatest.time.{ Seconds, Span, Millis }
import org.scalatest.{ GivenWhenThen, Matchers }

class LeaderIntegrationTest extends IntegrationFunSuite
    with MarathonClusterIntegrationTest
    with GivenWhenThen
    with Matchers
    with Eventually {
  implicit override val patienceConfig =
    PatienceConfig(timeout = scaled(Span(30, Seconds)), interval = scaled(Span(100, Millis)))

  test("all nodes return the same leader") {
    Given("a leader has been elected")
    eventually { marathon.leader.code should be (200) }

    When("calling /v2/leader on all nodes of a cluster")
    val results = marathonFacades.map(marathon => marathon.leader)

    Then("the requests should all be successful")
    results.foreach(_.code should be (200))

    And("they should all be the same")
    results.map(_.value).distinct should have length 1
  }

  test("the leader abdicates when it receives a DELETE") {
    Given("a leader")
    eventually { marathon.leader.code should be (200) }
    val leader = marathon.leader.value

    When("calling DELETE /v2/leader")
    val result = marathon.abdicate()

    Then("the request should be successful")
    result.code should be (200)
    (result.entityJson \ "message").as[String] should be ("Leadership abdicted")

    And("the leader must have changed")
    eventually { marathon.leader.value shouldNot be(leader) }
  }
}
