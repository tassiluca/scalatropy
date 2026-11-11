package it.unibo.pslab.motivation

import scala.concurrent.duration.{ DurationInt, FiniteDuration }

import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.deployment.Deployment.tiedTo
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.{ coAnisotropicComm, isotropicComm, on, take }
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

object BasicCommunication:

  type Light <: { type Tie <: via[MQTT toSingle Server] }
  type Server <: { type Tie <: via[MQTT toMultiple Light] }

  enum LightCommand derives ReadWriter:
    case On(intensity: Double)
    case Off
    case Dimm(intensity: Double, duration: FiniteDuration)

  case class Ack() derives ReadWriter

  enum LightRequest derives ReadWriter:
    case GetState()

  case class LightState(intensity: Double) derives ReadWriter

  import LightCommand.*
  import LightRequest.*

  def app[F[_]: {MonadThrow, Temporal, Console as console}](using MultiParty[F]): F[Unit] =
    for
      getStateRequest <- on[Server](GetState().pure[F])
      reqOnLight <- isotropicComm[Server, Light](getStateRequest)
      resOnLight <- on[Light](take(reqOnLight) >>= (_.serve))
      lightsStates <- coAnisotropicComm[Light, Server](resOnLight)

      onLightCommand <- on[Server](On(42.0).pure[F])
      cmdOnLight <- isotropicComm[Server, Light](onLightCommand)
      ackOnLight <- on[Light](take(cmdOnLight) >>= (_.handle))
      _ <- coAnisotropicComm[Light, Server](ackOnLight)
      _ <- on[Server](console.println("All lights acknowledged the command"))
    yield ()

  extension (req: LightCommand)
    def handle[F[_]: {Monad as m, Temporal as temporal, Console as console}]: F[Ack] = req match
      case On(intensity) => console.println(s"on: ${intensity}") >> temporal.sleep(5.seconds) >> m.pure(Ack())
      case Off           => console.println("off") >> m.pure(Ack())
      case Dimm(intensity, duration) =>
        console.println(s"dimm: ${intensity} for ${duration.toSeconds} seconds") >> m.pure(Ack())

  extension (req: GetState)
    def serve[F[_]: {Monad as m, Console as console}]: F[LightState] =
      console.println("Getting light state") >> m.pure(LightState(42.0))

import BasicCommunication.*

object BasicCommunicationApp extends IOApp.Simple:
  override def run: IO[Unit] = List(BasicCommunicationServerApp.run, BasicCommunicationLightApp.run).parSequence_

object BasicCommunicationServerApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Server](Configuration(appId = "rmi-playground"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(app[IO]).projectedOn[Server]:
        tiedTo[Light] via mqtt

object BasicCommunicationLightApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Light](Configuration(appId = "rmi-playground"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(app[IO]).projectedOn[Light]:
        tiedTo[Server] via mqtt
