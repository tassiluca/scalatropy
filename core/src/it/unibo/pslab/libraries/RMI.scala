package it.unibo.pslab.libraries

import scala.util.TupledFunction

import it.unibo.pslab.libraries.SyncLibrary.{ requestReplies, requestReply, scatterGather }
import it.unibo.pslab.multiparty.{ Label, MultiParty }
import it.unibo.pslab.multiparty.MultiParty.on
import it.unibo.pslab.network.{ Codable, CodableF }
import it.unibo.pslab.peers.Peers.{ CommunicationProtocolCompliance, PeerTag, TiedWithMultiple, TiedWithSingle }

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.syntax.all.*

object RMI:

  final class InvalidInvocation
      extends RuntimeException(
        "During RMI invocation an attempt to dereference the phantom peer receiver was not successful",
      )

  def dispatch[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: MonadThrow, Fun, Args <: Tuple, Res](
      methodInvocation: To => Fun,
  )(using
      party: MultiParty[F],
      tf: TupledFunction[Fun, Args => F[Res]],
      argsCodable: CodableF[F, Args],
      resCodable: CodableF[F, Res],
  )(arg: Args on From): F[Res on From] =
    dispatch[From, To](peer => tf.tupled(methodInvocation(peer)))(arg)

  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  def dispatch[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: MonadThrow, Arg: Codable[F], Res: Codable[F]](
      methodInvocation: To => Arg => F[Res],
  )(using party: MultiParty[F])(arg: Arg on From): F[Res on From] =
    requestReply[From, To](arg): request =>
      MonadThrow[F]
        .catchNonFatal(methodInvocation(summon[PeerTag[To]].asInstanceOf[To])(request))
        .flatten
        .adaptError { case _: ClassCastException => InvalidInvocation() }

  def dispatchMany[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: MonadThrow, Res: Codable[F]](
      methodInvocation: To => F[Res],
  )(using
      party: MultiParty[F],
      unitCodable: CodableF[F, Unit],
  ): F[party.Anisotropic[To, Res] on From] =
    for
      none <- on[From](().pure[F])
      res <- dispatchMany[From, To](peer => (_: Unit) => methodInvocation(peer))(none)
    yield res

  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  def dispatchMany[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: MonadThrow, Arg: Codable[F], Res: Codable[F]](
      methodInvocation: To => Arg => F[Res],
  )(using party: MultiParty[F])(arg: Arg on From): F[party.Anisotropic[To, Res] on From] =
    requestReplies[From, To](arg): request =>
      MonadThrow[F]
        .catchNonFatal(methodInvocation(summon[PeerTag[To]].asInstanceOf[To])(request))
        .flatten
        .adaptError { case _: ClassCastException => InvalidInvocation() }

  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  def scatterGatherDispatch[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: MonadThrow, Task: Codable[F], Res: Codable[F]](using
      party: MultiParty[F],
  )(
      allocator: Label[From] ?=> NonEmptyList[party.Remote[To]] => F[Map[party.Remote[To], Task]],
      default: Task,
  )(
      methodInvocation: To => Task => F[Res],
  ): F[party.Anisotropic[To, Res] on From] =
    scatterGather[From, To](
      allocator = allocator,
      default = default,
    ): task =>
      MonadThrow[F]
        .catchNonFatal(methodInvocation(summon[PeerTag[To]].asInstanceOf[To])(task))
        .flatten
        .adaptError { case _: ClassCastException => InvalidInvocation() }
