package it.unibo.pslab

import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.deployment.Deployment.tiedTo
import it.unibo.pslab.libraries.RMI.dispatch
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.on
import it.unibo.pslab.network.`*`
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.{ toSingle, via, Peer, PeerTag }

import cats.MonadThrow
import cats.effect.{ IO, IOApp }
import cats.effect.std.Console
import cats.syntax.all.*

object RMIPlayground:

  type Light <: { type Tie <: via[* toSingle Server] }
  type Server <: { type Tie <: via[* toSingle Light] }

  trait Powerable[F[_], P <: Peer]:
    extension (p: P)
      def on(lightValue: Double): F[Unit]
      def off(): F[Unit]

  given [F[_]: Console] => Powerable[F, Light] = new Powerable[F, Light]:
    extension (p: Light)
      override def off(): F[Unit] = F.println("Light off")
      override def on(lightValue: Double): F[Unit] = F.println(s"Light ${p.hashCode()} on with value $lightValue")

  def app[F[_]: {MonadThrow, Console}](using MultiParty[F]): F[Unit] =
    for
      lightValue <- on[Server](42.0.pure[F])
      _ <- dispatch[Server, Light](_.on)(lightValue)
    yield ()

import RMIPlayground.*

object RMIPlaygroundApp extends IOApp.Simple:
  override def run: IO[Unit] = List(RMIPlaygroundServerApp.run, RMIPlaygroundLightApp.run).parSequence_

object RMIPlaygroundServerApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Server](Configuration(appId = "rmi-playground"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(app[IO]).projectedOn[Server]:
        tiedTo[Light] via mqtt

object RMIPlaygroundLightApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Light](Configuration(appId = "rmi-playground"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(app[IO]).projectedOn[Light]:
        tiedTo[Server] via mqtt
