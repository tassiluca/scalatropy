package it.unibo.pslab.smartgarden

import it.unibo.pslab.smartgarden.SmartGardenArchitecture.*

import cats.Monad
import cats.effect.std.{ Console, Random }
import cats.syntax.all.*
import upickle.default.ReadWriter

/** A record representing the humidity measurement of a specific area. */
case class HumiditySample(area: Area, value: Double) derives ReadWriter

/** A record representing the battery status of a lawn mower. */
case class BatteryStatus(level: Int) derives ReadWriter

/** An area in the smart garden that can be mowed. */
case class Area(name: String) derives ReadWriter

/** The decision made by the smart garden system regarding whether to mow a specific area or skip it. */
enum MowingDecision derives ReadWriter:
  case Skip(reason: String)
  case Proceed

/** A record representing the result of the mowing decision process, including the decisions for each area. */
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

trait SoilSensing[F[_], P <: SoilMoistureSensor]:
  extension (p: P) def measureHumidity: F[HumiditySample]

trait MowerControl[F[_], P <: LawnMower]:
  extension (p: P)
    def batteryStatus: F[BatteryStatus]
    def mow(areas: Set[Area]): F[Unit]

trait UI[F[_], P <: Dashboard]:
  extension (p: P)
    def presentHumidityMeasurements(measurements: Set[HumiditySample]): F[Unit]
    def displayMowingResult(result: MowingResult): F[Unit]
    def displayMowingEnded: F[Unit]

/** The platform-specific capabilities required for the smart garden application. */
trait SmartGardenPlatform[F[_]]:
  given SoilSensing[F, SoilMoistureSensor] = compiletime.deferred

  given MowerControl[F, LawnMower] = compiletime.deferred

  given UI[F, Dashboard] = compiletime.deferred

/** A mocked implementation of the smart garden platform for testing and development purposes. */
trait SmartGardenMockedPlatform[F[_]: {Monad as m, Console as console, Random as rnd}] extends SmartGardenPlatform[F]:

  lazy val area = rnd.nextAlphaNumeric.replicateA(5).map(_.mkString).map(Area.apply)

  override given SoilSensing[F, SoilMoistureSensor] = new SoilSensing[F, SoilMoistureSensor]:
    extension (p: SoilMoistureSensor)
      def measureHumidity: F[HumiditySample] =
        val humidity = rnd.nextIntBounded(100).map(_ / 100.0)
        (area, humidity).mapN(HumiditySample.apply)

  override given MowerControl[F, LawnMower] = new MowerControl[F, LawnMower]:
    extension (p: LawnMower)
      def batteryStatus: F[BatteryStatus] = rnd.nextIntBounded(100).map(_ + 1).map(BatteryStatus(_))

      def mow(areas: Set[Area]): F[Unit] =
        if areas.isEmpty then console.println(s"[$p] No areas assigned for mowing")
        else console.println(s"[$p] Executing lawn mowing in zones '${areas.map(_.name).mkString(", ")}'")

  override given UI[F, Dashboard] = new UI[F, Dashboard]:
    import TermUtils.*

    extension (p: Dashboard)

      def presentHumidityMeasurements(measurements: Set[HumiditySample]): F[Unit] =
        val rows = measurements.toList
          .sortBy(_.area.name)
          .map: m =>
            val ratio = m.value
            val label = s"Area ${m.area}"
            s"${cyan(pad(label, 10))} ${gauge(ratio)}  ${status(ratio)}"
        console.println("\n" + panel(s"💧 Soil Humidity · $p", rows) + "\n")

      def displayMowingResult(result: MowingResult): F[Unit] =
        val entries = result.decisions.toList.sortBy(_._1.toString)
        val rows = entries.map: (area, decision) =>
          decision match
            case MowingDecision.Proceed      => s"${green("✔")} ${bold(pad(area.toString, 12))} ${green("Proceed")}"
            case MowingDecision.Skip(reason) =>
              s"${yellow("✘")} ${bold(pad(area.toString, 12))} ${yellow(s"Skip: $reason")}"
        console.println("\n" + panel(s"🌱 Mowing Result · $p", rows) + "\n")

      def displayMowingEnded: F[Unit] =
        console.println(
          "\n" + panel(s"🏁 Done · $p", bold("Mowing process ended!") :: Nil) + "\n",
        )
