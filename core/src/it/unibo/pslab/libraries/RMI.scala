package it.unibo.pslab.libraries

import scala.util.TupledFunction

import it.unibo.pslab.libraries.SyncLibrary.{ requestReplies, requestReply, scatterGather }
import it.unibo.pslab.multiparty.{ Label, MultiParty }
import it.unibo.pslab.multiparty.MultiParty.on
import it.unibo.pslab.network.{ Codable, CodableF }
import it.unibo.pslab.network.CodableInstances.given
import it.unibo.pslab.peers.Peers.{ CommunicationProtocolCompliance, PeerTag, TiedWithMultiple, TiedWithSingle }

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.syntax.all.*

object RMI:

  final class InvalidInvocation
      extends RuntimeException(
        "During RMI invocation an attempt to dereference the phantom peer receiver was not successful",
      )

  /** Overload of [[it.unibo.pslab.libraries.RMI.dispatch]] for remote calls with no arguments. */
  def dispatch[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: MonadThrow, Res: Codable[F]](remoteCall: To => F[Res])(using MultiParty[F]): F[Res on From] =
    for
      unit <- on[From](().pure[F])
      res <- dispatch[From, To](peer => (_: Unit) => remoteCall(peer))(unit)
    yield res

  /** Generalized version of [[it.unibo.pslab.libraries.RMI.dispatch]] for remote calls with multiple arguments. */
  def dispatch[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: MonadThrow, Fun, Args <: Tuple, Res](remoteCall: To => Fun)(args: Args on From)(using
      party: MultiParty[F],
      tf: TupledFunction[Fun, Args => F[Res]],
      argsCodable: CodableF[F, Args],
      resCodable: CodableF[F, Res],
  ): F[Res on From] =
    dispatch[From, To](peer => tf.tupled(remoteCall(peer)))(args)

  /**
   * Dispatch a remote function call to the single peer of type `To` tied to `From`.
   *
   * @param remoteCall
   *   the remote call to dispatch
   * @param arg
   *   the argument to pass to the remote call
   * @return
   *   the effectful result of the remote call on `To`.
   */
  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  def dispatch[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: MonadThrow, Arg: Codable[F], Res: Codable[F]](
      remoteCall: To => Arg => F[Res],
  )(using party: MultiParty[F])(arg: Arg on From): F[Res on From] =
    requestReply[From, To](arg): request =>
      MonadThrow[F]
        .catchNonFatal(remoteCall(summon[PeerTag[To]].asInstanceOf[To])(request))
        .flatten
        .adaptError { case _: ClassCastException => InvalidInvocation() }

  /** Overload of [[it.unibo.pslab.libraries.RMI.dispatchMany]] for remote calls with no arguments. */
  def dispatchMany[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: MonadThrow, Res: Codable[F]](
      remoteCall: To => F[Res],
  )(using party: MultiParty[F]): F[party.Anisotropic[To, Res] on From] =
    for
      none <- on[From](().pure[F])
      res <- dispatchMany[From, To](peer => (_: Unit) => remoteCall(peer))(none)
    yield res

  /**
   * Generalized version of [[it.unibo.pslab.libraries.RMI.dispatchMany]] supporting remote calls with multiple
   * arguments.
   */
  def dispatchMany[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: MonadThrow, Fun, Args <: Tuple, Res: Codable[F]](
      remoteCall: To => Fun,
  )(args: Args on From)(using
      party: MultiParty[F],
      tf: TupledFunction[Fun, Args => F[Res]],
      argsCodable: CodableF[F, Args],
  ): F[party.Anisotropic[To, Res] on From] =
    dispatchMany[From, To](peer => tf.tupled(remoteCall(peer)))(args)

  /**
   * Dispatch a remote function to all peers of type `To` tied to `From`.
   *
   * @param remoteCall
   *   the remote call to dispatch
   * @param allocator
   *   the function that allocates tasks to each peer of type `To`
   * @param default
   *   the default task to be sent to peers that are not allocated any task
   * @return
   *   the effectful result of the remote call on each peer of type `To`.
   */
  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  def dispatchMany[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: MonadThrow, Arg: Codable[F], Res: Codable[F]](
      remoteCall: To => Arg => F[Res],
  )(using party: MultiParty[F])(arg: Arg on From): F[party.Anisotropic[To, Res] on From] =
    requestReplies[From, To](arg): request =>
      MonadThrow[F]
        .catchNonFatal(remoteCall(summon[PeerTag[To]].asInstanceOf[To])(request))
        .flatten
        .adaptError { case _: ClassCastException => InvalidInvocation() }

  /**
   * Performs a scatter-gather operation, performed by dispatching a remote function call to all peers of type `To` tied
   * to `From`, and gathering the results.
   *
   * @param allocator
   *   the effectful function responsible for allocating tasks to the available peers.
   * @param default
   *   the default task to be used in case a peer is not allocated a specific task.
   * @param remoteCall
   *   the effectful function call to dispatch
   * @return
   *   the effectful result of the remote call on each peer of type `To`, gathered on the peer of type `From`.
   */
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
      remoteCall: To => Task => F[Res],
  ): F[party.Anisotropic[To, Res] on From] =
    scatterGather[From, To](
      allocator = allocator,
      default = default,
    ): task =>
      MonadThrow[F]
        .catchNonFatal(remoteCall(summon[PeerTag[To]].asInstanceOf[To])(task))
        .flatten
        .adaptError { case _: ClassCastException => InvalidInvocation() }
