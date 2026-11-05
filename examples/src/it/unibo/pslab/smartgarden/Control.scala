package it.unibo.pslab.smartgarden

import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.libraries.RMI.{ dispatchMany, scatterGatherDispatch }
import it.unibo.pslab.multiparty.{ Label, MultiParty }
import it.unibo.pslab.multiparty.MultiParty.{ on, select, take, takeAll }
import it.unibo.pslab.smartgarden.MowingDecision.{ Proceed, Skip }
import it.unibo.pslab.smartgarden.SmartGardenArchitecture.*

import cats.MonadThrow
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

trait SmartGarden[F[_]: {MonadThrow, Console}]:
  self: SmartGardenRequirements[F] =>

  def mowingApp(using MultiParty[F]): F[Unit] =
    for
      aggregateHumidity <- dispatchMany[Server, SoilMoistureSensor](_.measure)
      mowingResult <- on[Server](evaluateWhetherMowing(aggregateHumidity))
      skip <- on[Server](take(mowingResult).map(_.allSkipped))
      _ <- select[Server](skip):
        case true  => F.println("All zones are skipped")
        case false => executeMowing(mowingResult)
    yield ()

  private def evaluateWhetherMowing(using
      l: MultiParty[F],
      server: Label[Server],
  )(measures: l.Anisotropic[SoilMoistureSensor, Humidity] on Server): F[MowingResult] =
    for
      measurements <- takeAll(measures)
      _ <- Console[F].println(s"Received humidity values: $measurements")
      decisions = measurements.map:
        case (_, measure) =>
          Area(measure.area) -> (if measure.value >= 0.5 then Proceed else Skip("Humidity below threshold"))
    yield MowingResult(decisions)

  private def executeMowing(using lang: MultiParty[F])(mowingResult: MowingResult on Server): F[Unit] =
    for
      _ <- Console[F].println(s"Handling process for decisions: $mowingResult")
      batteryStatus <- dispatchMany[Server, LawnMower](_.measure)
      availableMowers <- on[Server]:
        takeAll(batteryStatus) >>= (_.filter { case (_, status) => status.level > 20 }.pure[F])
      _ <- scatterGatherDispatch[Server, LawnMower](
        allocator = mowers => take(mowingResult).map(_.allocateTo(mowers.toList)),
        default = Set.empty[Area],
      )(_.execute)
    yield ()
