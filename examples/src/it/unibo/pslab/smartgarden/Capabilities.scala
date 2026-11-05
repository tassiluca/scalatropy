package it.unibo.pslab.smartgarden

import it.unibo.pslab.peers.Peers.Peer
import it.unibo.pslab.smartgarden.SmartGardenArchitecture.*

import cats.Monad
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

trait Measurable[F[_], P <: Peer, Measurement]:
  extension (p: P) def measure: F[Measurement]

trait Actuatable[F[_], P <: Peer, Action]:
  extension (p: P) def execute(action: Action): F[Unit]

case class Humidity(area: String, value: Double) derives ReadWriter

case class BatteryStatus(level: Int) derives ReadWriter

case class Area(name: String) derives ReadWriter
enum MowingDecision derives ReadWriter:
  case Skip(reason: String)
  case Proceed

case class MowingResult(decisions: Map[Area, MowingDecision]) derives ReadWriter:
  def allSkipped: Boolean = decisions.values.forall:
    case MowingDecision.Skip(_) => true
    case MowingDecision.Proceed => false

  def areasToMow: Iterable[Area] = decisions.collect:
    case (area, MowingDecision.Proceed) => area

  def allocateTo[P](instances: Seq[P]): Map[P, Set[Area]] =
    areasToMow.zipWithIndex
      .groupMap((_, i) => instances(i % instances.size))((area, _) => area)
      .map { case (peer, areas) => peer -> areas.toSet }

trait SmartGardenRequirements[F[_]]:
  given humiditySensor: Measurable[F, SoilMoistureSensor, Humidity] = compiletime.deferred

  given batteryChecker: Measurable[F, LawnMower, BatteryStatus] = compiletime.deferred

  given lawnMower: Actuatable[F, LawnMower, Set[Area]] = compiletime.deferred

trait SmartGardenMockedInstances[F[_]](using Console[F], Monad[F]) extends SmartGardenRequirements[F]:

  override given humiditySensor: Measurable[F, SoilMoistureSensor, Humidity] =
    new Measurable[F, SoilMoistureSensor, Humidity]:
      extension (p: SoilMoistureSensor)
        def measure: F[Humidity] =
          Console[F].println(s"Measuring soil moisture with $p") *> Monad[F].pure(Humidity("area1", 0.9))

  override given batteryChecker: Measurable[F, LawnMower, BatteryStatus] =
    new Measurable[F, LawnMower, BatteryStatus]:
      extension (p: LawnMower)
        def measure: F[BatteryStatus] =
          Console[F].println(s"Measuring battery status with $p") *> Monad[F].pure(BatteryStatus(100))

  override given lawnMower: Actuatable[F, LawnMower, Set[Area]] =
    new Actuatable[F, LawnMower, Set[Area]]:
      extension (p: LawnMower)
        def execute(areas: Set[Area]): F[Unit] =
          if areas.isEmpty then Console[F].println(s"No areas assigned to $p for mowing")
          else Console[F].println(s"Executing lawn mowing in zones '${areas.map(_.name).mkString(", ")}' with $p")
