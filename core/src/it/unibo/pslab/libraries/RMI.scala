package it.unibo.pslab.libraries

import it.unibo.pslab.libraries.SyncLibrary.requestReply
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.on
import it.unibo.pslab.network.Codable
import it.unibo.pslab.peers.Peers.{ CommunicationProtocolCompliance, PeerTag, TiedWithSingle }

import cats.MonadThrow
import cats.syntax.all.*

object RMI:

  final class InvalidInvocation
      extends RuntimeException(
        "During RMI invocation an attempt to dereference the phantom peer receiver was not successful",
      )

  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  def dispatch[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: MonadThrow](using
      party: MultiParty[F],
  )[Res: Codable[F], Fun, Arg: Codable[F]](methodInvocation: To => Arg => F[Res])(arg: Arg on From): F[Res on From] =
    requestReply[From, To](arg): request =>
      MonadThrow[F]
        .catchNonFatal(methodInvocation(summon[PeerTag[To]].asInstanceOf[To])(request))
        .flatten
        .adaptError { case _: ClassCastException => InvalidInvocation() }
