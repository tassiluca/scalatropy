package it.unibo.pslab.smartgarden

import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.deployment.Deployment.tiedTo
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.smartgarden.SmartGardenArchitecture.*

import cats.effect.{ IO, IOApp }

object SmartGardenApp extends IOApp.Simple:
  override def run: IO[Unit] =
    (
      SmartGardenServerApp.run ::
        SmartGardenSoilMoistureSensorApp.run ::
        List.fill(3)(SmartGardenLawnMowerApp.run)
    ).parSequence_

object SmartGardenServerApp extends IOApp.Simple with SmartGarden[IO] with SmartGardenMockedInstances[IO]:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Server](Configuration(appId = "smart-garden"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(mowingApp).projectedOn[Server]:
        tiedTo[Device] via mqtt

object SmartGardenSoilMoistureSensorApp extends IOApp.Simple with SmartGarden[IO] with SmartGardenMockedInstances[IO]:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, SoilMoistureSensor](Configuration(appId = "smart-garden"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(mowingApp).projectedOn[SoilMoistureSensor]:
        tiedTo[Server] via mqtt

object SmartGardenLawnMowerApp extends IOApp.Simple with SmartGarden[IO] with SmartGardenMockedInstances[IO]:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, LawnMower](Configuration(appId = "smart-garden"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(mowingApp).projectedOn[LawnMower]:
        tiedTo[Server] via mqtt
