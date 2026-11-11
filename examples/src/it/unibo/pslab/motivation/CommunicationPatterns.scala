package it.unibo.pslab.motivation

import scala.concurrent.duration.{ DurationInt, FiniteDuration }

import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.deployment.Deployment.tiedTo
import it.unibo.pslab.libraries.all.*
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.on
import it.unibo.pslab.network.MQTT
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.{ toMultiple, toSingle, via }

import cats.{ Monad, MonadThrow }
import cats.effect.{ IO, IOApp }
import cats.effect.kernel.Temporal
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

object CommunicationPatterns:

  type Light <: { type Tie <: via[MQTT toSingle Server] }
  type Server <: { type Tie <: via[MQTT toMultiple Light] }

  enum LightCommand derives ReadWriter:
    case On(intensity: Double)
    case Off
    case Dimm(intensity: Double, duration: FiniteDuration)

  enum LightRequest derives ReadWriter:
    case GetState()

  case class LightState(intensity: Double) derives ReadWriter

  import LightCommand.*
  import LightRequest.*

  def app[F[_]: {MonadThrow, Temporal, Console}](using MultiParty[F]): F[Unit] =
    for
      getStateRequest <- on[Server](GetState().pure[F])
      allLightsStates <- requestReplies[Server, Light](getStateRequest)(_.request)
      onLightRequest <- on[Server](On(42.0).pure[F])
      response <- requestReplies[Server, Light](onLightRequest)(_.handle)
    yield ()

  extension (req: LightCommand)
    def handle[F[_]: {Temporal as temporal, Console as console}]: F[Unit] = req match
      case On(intensity) =>
        console.println(s"on: ${intensity}") >> temporal.sleep(5.seconds) >> console.println("on command completed")
      case Off                       => console.println("off")
      case Dimm(intensity, duration) => console.println(s"dimm: ${intensity} for ${duration.toSeconds} seconds")

  extension (req: GetState)
    def request[F[_]: {Monad as m, Console as console}]: F[LightState] =
      console.println("Getting light state") >> m.pure(LightState(42.0))

import CommunicationPatterns.*

object CommunicationPatternsApp extends IOApp.Simple:
  override def run: IO[Unit] = List(CommunicationPatternsServerApp.run, CommunicationPatternsLightApp.run).parSequence_

object CommunicationPatternsServerApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Server](Configuration(appId = "rmi-playground"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(app[IO]).projectedOn[Server]:
        tiedTo[Light] via mqtt

object CommunicationPatternsLightApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Light](Configuration(appId = "rmi-playground"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(app[IO]).projectedOn[Light]:
        tiedTo[Server] via mqtt
