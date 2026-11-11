package it.unibo.pslab.motivation

import scala.concurrent.duration.FiniteDuration

import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.deployment.Deployment.tiedTo
import it.unibo.pslab.libraries.all.*
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.on
import it.unibo.pslab.network.MQTT
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.{ toMultiple, toSingle, via, Peer }

import cats.{ Monad, MonadThrow }
import cats.effect.{ IO, IOApp }
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

object RMI:

  type Light <: { type Tie <: via[MQTT toSingle Server] }
  type Server <: { type Tie <: via[MQTT toMultiple Light] }

  case class LightState(intensity: Double) derives ReadWriter

  trait Powerable[F[_], P <: Peer]:
    extension (p: P)
      def on(lightValue: Double): F[Unit]
      def dim(lightValue: Double, duration: FiniteDuration): F[Unit]
      def off: F[Unit]
      def state: F[LightState]

  given [F[_]: {Monad as m, Console as console}] => Powerable[F, Light] = new Powerable[F, Light]:
    extension (p: Light)
      override def state: F[LightState] = console.println("Getting light state") >> m.pure(LightState(42.0))
      override def off: F[Unit] = console.println("Light off")
      override def on(lightValue: Double): F[Unit] = console.println(s"Light ${p.hashCode()} on with value $lightValue")
      override def dim(lightValue: Double, duration: FiniteDuration): F[Unit] =
        console.println(s"Light ${p.hashCode()} dim with value $lightValue for duration $duration")

  def app[F[_]: {MonadThrow, Console}](using MultiParty[F]): F[Unit] =
    for
      allLightsStates <- dispatchMany[Server, Light](_.state)
      lightValue <- on[Server](42.0.pure[F])
      _ <- dispatchMany[Server, Light](_.on)(lightValue)
    yield ()

import RMI.*

object RMIApp extends IOApp.Simple:
  override def run: IO[Unit] = List(RMIServerApp.run, RMILightApp.run).parSequence_

object RMIServerApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Server](Configuration(appId = "rmi-playground"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(app[IO]).projectedOn[Server]:
        tiedTo[Light] via mqtt

object RMILightApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Light](Configuration(appId = "rmi-playground"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(app[IO]).projectedOn[Light]:
        tiedTo[Server] via mqtt
