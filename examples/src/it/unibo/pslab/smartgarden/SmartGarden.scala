package it.unibo.pslab.smartgarden

import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.libraries.all.*
import it.unibo.pslab.multiparty.{ Label, MultiParty }
import it.unibo.pslab.multiparty.MultiParty.{ on, select, take, takeAll }
import it.unibo.pslab.smartgarden.MowingDecision.{ Proceed, Skip }
import it.unibo.pslab.smartgarden.SmartGardenArchitecture.*

import cats.MonadThrow
import cats.syntax.all.*
import upickle.default.ReadWriter

trait SmartGarden[F[_]: MonadThrow]:
  self: SmartGardenPlatform[F] =>

  def mowingApp(using MultiParty[F]): F[Unit] =
    for
      humidities <- dispatchMany[Server, SoilMoistureSensor](_.measureHumidity)
      humitidyValues <- on[Server](takeAll(humidities) >>= (_.values.toSet.pure[F]))
      _ <- dispatch[Server, Dashboard](_.displayHumidity)(humitidyValues)
      mowingResult <- on[Server](shouldMow(humitidyValues))
      _ <- dispatch[Server, Dashboard](_.displayMowingResult)(mowingResult)
      skip <- on[Server](take(mowingResult).map(_.allSkipped))
      _ <- select[Server](skip):
        case false => executeMowing(mowingResult)
        case _     => ().pure[F]
    yield ()

  private def shouldMow(
      humidityValues: Set[HumiditySample] on Server,
  )(using MultiParty[F], Label[Server]): F[MowingResult] =
    for
      measurements <- take(humidityValues)
      decisions = measurements.map: m =>
        m.area -> (if m.value <= 0.4 then Proceed else Skip("Humidity above threshold!"))
    yield MowingResult(decisions.toMap)

  private def executeMowing(using lang: MultiParty[F])(mowingResult: MowingResult on Server): F[Unit] =
    for
      batteryStatus <- dispatchMany[Server, LawnMower](_.batteryStatus)
      availableMowers <- on[Server]:
        takeAll(batteryStatus) >>= (_.values.filter(_.level > 20).pure[F])
      _ <- scatterGatherDispatch[Server, LawnMower](
        allocator = mowers => take(mowingResult).map(_.allocateTo(mowers.toList)),
        default = Set.empty[Area],
      )(_.mow)
      _ <- dispatch[Server, Dashboard](_.displayMowingEnded)
    yield ()
