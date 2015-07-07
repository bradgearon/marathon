package mesosphere.marathon.integration

import mesosphere.marathon.integration.setup.{ IntegrationFunSuite, SingleMarathonIntegrationTest }
import org.scalatest._
import org.scalatest.concurrent.Eventually
import org.scalatest.time.{ Millis, Seconds, Span }

class EventSubscribersIntegrationTest
    extends IntegrationFunSuite
    with SingleMarathonIntegrationTest
    with Matchers
    with BeforeAndAfter
    with GivenWhenThen
    with Eventually {
  implicit override val patienceConfig =
    PatienceConfig(timeout = scaled(Span(65, Seconds)), interval = scaled(Span(100, Millis)))

  before(cleanUp())

  test("adding an event subscriber") {
    When("an event subscriber is added")
    marathon.subscribe("http://localhost:1337").code should be(200)

    Then("a notification should be sent to all the subscribers")
    eventually { waitForEvent("subscribe_event").info should contain("callbackUrl" -> "http://localhost:1337") }

    And("the subscriber should show up in the list of subscribers")
    marathon.listSubscribers.value.urls should contain("http://localhost:1337")

    // Cleanup
    marathon.unsubscribe("http://localhost:1337")
  }

  test("removing an event subscriber") {
    When("an event subscriber is removed")
    marathon.subscribe("http://localhost:1337").code should be(200)
    marathon.listSubscribers.value.urls should contain("http://localhost:1337")
    marathon.unsubscribe("http://localhost:1337").code should be(200)

    Then("a notification should be sent to all the subscribers")
    eventually { waitForEvent("unsubscribe_event").info should contain("callbackUrl" -> "http://localhost:1337") }

    And("the subscriber should not show up in the list of subscribers")
    marathon.listSubscribers.value.urls shouldNot contain("http://localhost:1337")
  }
}
