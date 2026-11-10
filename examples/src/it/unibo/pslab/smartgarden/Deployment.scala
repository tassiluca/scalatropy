package it.unibo.pslab.smartgarden

import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.deployment.Deployment.tiedTo
import it.unibo.pslab.network.PeerId
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.network.ws.WebSocketNetwork
import it.unibo.pslab.smartgarden.SmartGardenArchitecture.*

import cats.effect.{ IO, IOApp }
import cats.syntax.all.*
import com.comcast.ip4s.*

def mqttConfig = Configuration(appId = "smart-home")
def wsConfig = Map(
  PeerId[Server]("server") -> SocketAddress(ipv4"127.0.0.1", port"10001"),
  PeerId[Dashboard]("dashboard") -> SocketAddress(ipv4"127.0.0.1", port"10000"),
)

object SmartGardenApp extends IOApp.Simple:
  override def run: IO[Unit] =
    (
      (SmartGardenServerApp.run :: SmartGardenDashboardApp.run :: Nil) ++
        List.fill(3)(SmartGardenSoilMoistureSensorApp.run) ++
        List.fill(3)(SmartGardenLawnMowerApp.run)
    ).parSequence_

object SmartGardenServerApp extends IOApp.Simple with SmartGarden[IO] with SmartGardenMockedPlatform[IO]():
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Server](Configuration(appId = "smart-garden"))
    val wsNetwork = WebSocketNetwork.make[IO, Server](id = "server", port = port"10001", knownPeers = wsConfig)
    (mqttNetwork, wsNetwork).tupled.use: (mqtt, ws) =>
      ScalaTropy(mowingApp).projectedOn[Server]:
        tiedTo[Device] via mqtt
        tiedTo[Dashboard] via ws

object SmartGardenDashboardApp extends IOApp.Simple with SmartGarden[IO] with SmartGardenMockedPlatform[IO]():
  override def run: IO[Unit] =
    val wsNetwork = WebSocketNetwork.make[IO, Dashboard](id = "dashboard", port = port"10000", knownPeers = wsConfig)
    wsNetwork.use: ws =>
      ScalaTropy(mowingApp).projectedOn[Dashboard]:
        tiedTo[Server] via ws

object SmartGardenSoilMoistureSensorApp extends IOApp.Simple with SmartGarden[IO] with SmartGardenMockedPlatform[IO]:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, SoilMoistureSensor](Configuration(appId = "smart-garden"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(mowingApp).projectedOn[SoilMoistureSensor]:
        tiedTo[Server] via mqtt

object SmartGardenLawnMowerApp extends IOApp.Simple with SmartGarden[IO] with SmartGardenMockedPlatform[IO]():
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, LawnMower](Configuration(appId = "smart-garden"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(mowingApp).projectedOn[LawnMower]:
        tiedTo[Server] via mqtt
