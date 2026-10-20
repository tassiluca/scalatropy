package it.unibo.pslab

import it.unibo.pslab.peers.Peers.via
import it.unibo.pslab.peers.Peers.toSingle
import it.unibo.pslab.peers.Peers.Peer
import it.unibo.pslab.network.`*`
import it.unibo.pslab.peers.Peers.PeerTag
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.libraries.RMI.dispatch
import cats.effect.std.Console
import it.unibo.pslab.multiparty.MultiParty.on
import cats.syntax.all.*
import it.unibo.pslab.UpickleCodable.given
import cats.MonadThrow
import cats.effect.IOApp
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import cats.effect.IO
import it.unibo.pslab.deployment.Deployment.tiedTo

object RMIPlayground:

  type Light <: { type Tie <: via[* toSingle Server] }
  type Server <: { type Tie <: via[* toSingle Light] }

  case class LightSchedule(from: Double, to: Double, lightValue: Double)

  trait Powerable[F[_], P <: Peer]:
    extension (p: P)
      def on(lightValue: Double): F[Unit]
      def dim(lightValue: Double, duration: Double): F[Unit]
      def schedule(schedule: LightSchedule): F[Unit]
      def off(): F[Unit]

  given [F[_]: Console] => Powerable[F, Light] = new Powerable[F, Light] {
    extension (p: Light)
      override def off(): F[Unit] = F.println("Light off")
      override def on(lightValue: Double): F[Unit] = F.println(s"Light on with value $lightValue")
      override def dim(lightValue: Double, duration: Double): F[Unit] =
        F.println(s"Light dimmed to $lightValue for $duration")
      def schedule(schedule: LightSchedule): F[Unit] =
        F.println(s"Light scheduled from ${schedule.from} to ${schedule.to} with value ${schedule.lightValue}")
  }

  def app[F[_]: {MonadThrow, Console}](using MultiParty[F]): F[Unit] =
    for
      lightValue <- on[Server](42.0.pure[F])
      _ <- dispatch[Server, Light](_.on)(lightValue)
    yield ()

  ///////////////////////////////////////////////////////

  // def foo[P <: Peer](using
  //     PeerTag[P],
  // )[V, Res](thunk: P => V => Res)(v: V): Res = ???

  // def foo[P <: Peer](using
  //     PeerTag[P],
  // )[F, Args <: Tuple, Res](thunk: P => F)(args: Args)(using
  //     tf: TupledFunction[F, Args => Res],
  // ): Res = ???

  // @main def runUselessTest(): Unit =
  //   val p: Light = null.asInstanceOf[Light]
  //   foo[Light](_.on)(42.0)
  //   foo[Light](_.dim)((0.5, 2.0))
  //   foo[Light](_.schedule)(LightSchedule(0.0, 1.0, 42.0))
  //   p.on(42.0)
  //   p.off()

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
