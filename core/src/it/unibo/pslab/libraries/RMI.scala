package it.unibo.pslab.libraries

import it.unibo.pslab.peers.Peers.TiedWithSingle
import it.unibo.pslab.peers.Peers.PeerTag
import it.unibo.pslab.peers.Peers.CommunicationProtocolCompliance
import cats.Monad
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.on
import it.unibo.pslab.network.Codable
import it.unibo.pslab.libraries.SyncLibrary.requestReply

object RMI:

  def dispatch[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: Monad](using
      party: MultiParty[F],
  )[Res: Codable[F], Fun, Arg: Codable[F]](methodInvocation: To => Arg => F[Res])(arg: Arg on From): F[Res on From] =
    requestReply[From, To](arg)(methodInvocation(null.asInstanceOf[To]))
