package outwatch

import cats.{Bifunctor, Functor, Monoid}
import cats.effect.{IO, Sync, SyncIO}
import colibri._
import colibri.effect._

import org.scalajs.dom

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration

// The EmitterBuilder[O, R] allows you to build an R that produces values of type O.
// The builder gives you a declarative interface to describe transformations on the
// emitted values of type O and the result type R.
//
// Example onClick event:
// onClick: EmitterBuilder[ClickEvent, Emitter]

// The result emitter describes a registration of the click event on the embedding
// dom element. This produces click events that can be transformed:
// onClick.map(_ => 1): EmitterBuilder[Int, Emitter]

// We keep the same result, the registration for the click event, but map the emitted
// click events to integers. You can also map the result type:
// onClick.mapResult(emitter => VMod(emitter, ???)): EmitterBuilder[Int, VMod]
//
// Now you have combined the emitter with another VMod, so the combined modifier
// will later be rendered instead of only the emitter. Then you can describe the action
// that should be done when an event triggers:
//
// onClick.map(_ => 1).doAction(doSomething(_)): VMod
//
// The EmitterBuilder result must be a SubscriptionOwner to handle the subscription
// from the EmitterBuilder.
//

trait EmitterBuilder[+O, +R] extends AttrBuilder[O => Unit, R] {
  @inline def forwardTo[F[_]: Sink, O2 >: O](sink: F[O2]): R

  @inline def transform[T](f: Observable[O] => Observable[T]): EmitterBuilder[T, R]
  @inline def transformSink[T](f: Observer[T] => Observer[O]): EmitterBuilder[T, R]

  @inline final def -->(sink: Observer[O]): R                                                     = forwardTo(sink)
  @inline final def -->[F[_]: Sink, O2 >: O](sink: F[O2], @annotation.nowarn dummy: Unit = ()): R = forwardTo(sink)

  @inline final def done: R = forwardTo(Observer.empty)

  @inline final def foreach(action: O => Unit): R = forwardTo(Observer.create(action))
  @inline final def doAction(action: => Unit): R  = foreach(_ => action)

  @inline final def assign(action: O => Unit): R = foreach(action)

  @inline def foreachEffect[G[_]: RunEffect](action: O => G[Unit]): R = mapEffect(action).done
  @inline def doEffect[G[_]: RunEffect](action: => G[Unit]): R        = foreachEffect(_ => action)

  @inline def foreachFuture(action: O => Future[Unit]): R = mapFuture(action).done
  @inline def doFuture(action: => Future[Unit]): R        = foreachFuture(_ => action)

  @inline def foreachSingleEffect[G[_]: RunEffect](action: O => G[Unit]): R =
    singleMapEffect(action).done
  @inline def doSingleEffect[G[_]: RunEffect](action: G[Unit]): R =
    foreachSingleEffect(_ => action)

  @inline def foreachSingleFuture(action: O => Future[Unit]): R =
    singleMapFuture(action).done
  @inline def doSingleFuture(action: => Future[Unit]): R =
    foreachSingleFuture(_ => action)

  @inline def foreachSwitchEffect[G[_]: RunEffect](action: O => G[Unit]): R =
    switchMapEffect(action).done
  @inline def doSwitchEffect[G[_]: RunEffect](action: G[Unit]): R =
    foreachSwitchEffect(_ => action)

  @inline def foreachSwitchFuture(action: O => Future[Unit]): R =
    switchMapFuture(action).done
  @inline def doSwitchFuture(action: => Future[Unit]): R =
    foreachSwitchFuture(_ => action)

  @inline def foreachParEffect[G[_]: RunEffect](action: O => G[Unit]): R =
    parMapEffect(action).done
  @inline def doParEffect[G[_]: RunEffect](action: G[Unit]): R =
    foreachParEffect(_ => action)

  @inline def foreachParFuture(action: O => Future[Unit]): R =
    parMapFuture(action).done
  @inline def doParFuture(action: Future[Unit]): R =
    foreachParFuture(_ => action)

  @inline def via[F[_]: Sink, O2 >: O](sink: F[O2]): EmitterBuilder[O, R] =
    transformSink[O](Observer.combine(_, Observer.lift(sink)))
  @inline def dispatchWith(dispatcher: EventDispatcher[O]): R = transform(dispatcher.dispatch).done

  @inline final def map[T](f: O => T): EmitterBuilder[T, R] = transformSink(_.contramap(f))

  @inline final def tap(f: O => Unit): EmitterBuilder[O, R] = transformSink(_.tap(f))

  @inline final def tapEffect[F[_]: RunEffect: Functor](f: O => F[Unit]): EmitterBuilder[O, R] = transform(
    _.tapEffect(f),
  )

  @inline final def collect[T](f: PartialFunction[O, T]): EmitterBuilder[T, R] = transformSink(
    _.contracollect(f),
  )

  @inline final def filter(predicate: O => Boolean): EmitterBuilder[O, R] = transformSink(
    _.contrafilter(predicate),
  )

  @inline final def mapFilter[T](f: O => Option[T]): EmitterBuilder[T, R] = transformSink(
    _.contramapFilter(f),
  )

  @inline final def mapIterable[T](f: O => Iterable[T]): EmitterBuilder[T, R] = transformSink(
    _.contramapIterable(f),
  )

  @inline final def as[T](value: T): EmitterBuilder[T, R]        = map(_ => value)
  @inline final def asEval[T](value: => T): EmitterBuilder[T, R] = map(_ => value)

  @inline final def void: EmitterBuilder[Unit, R] = as(())

  @inline final def asEffect[G[_]: RunEffect, T](value: => G[T]): EmitterBuilder[T, R] = mapEffect(_ => value)
  @inline final def withEffect[G[_]: RunEffect: Functor, T](value: G[T]): EmitterBuilder[(O, T), R] =
    mapEffect(x => Functor[G].map(value)(x -> _))

  @inline final def asFuture[T](value: => Future[T]): EmitterBuilder[T, R] = mapFuture(_ => value)
  @inline final def withFuture[T](value: => Future[T]): EmitterBuilder[(O, T), R] =
    mapFuture(x => value.map(x -> _)(ExecutionContext.parasitic))

  @inline final def asSingleEffect[G[_]: RunEffect, T](value: => G[T]): EmitterBuilder[T, R] =
    singleMapEffect(_ => value)
  @inline final def withSingleEffect[G[_]: RunEffect: Functor, T](value: G[T]): EmitterBuilder[(O, T), R] =
    singleMapEffect(x => Functor[G].map(value)(x -> _))

  @inline final def asSingleFuture[T](value: => Future[T]): EmitterBuilder[T, R] =
    singleMapFuture(_ => value)
  @inline final def withSingleFuture[T](value: => Future[T]): EmitterBuilder[(O, T), R] =
    withSingleEffect(IO.fromFuture(IO(value)))

  @inline final def asSwitchEffect[G[_]: RunEffect, T](value: => G[T]): EmitterBuilder[T, R] =
    switchMapEffect(_ => value)
  @inline final def withSwitchEffect[G[_]: RunEffect: Functor, T](value: G[T]): EmitterBuilder[(O, T), R] =
    switchMapEffect(x => Functor[G].map(value)(x -> _))

  @inline final def asSwitchFuture[T](value: => Future[T]): EmitterBuilder[T, R] =
    switchMapFuture(_ => value)
  @inline final def withSwitchFuture[T](value: => Future[T]): EmitterBuilder[(O, T), R] =
    withSwitchEffect(IO.fromFuture(IO(value)))

  @inline final def asParEffect[G[_]: RunEffect, T](value: => G[T]): EmitterBuilder[T, R] =
    parMapEffect(_ => value)
  @inline final def withParEffect[G[_]: RunEffect: Functor, T](value: G[T]): EmitterBuilder[(O, T), R] =
    parMapEffect(x => Functor[G].map(value)(x -> _))

  @inline final def asParFuture[T](value: => Future[T]): EmitterBuilder[T, R] =
    parMapFuture(_ => value)
  @inline final def withParFuture[T](value: => Future[T]): EmitterBuilder[(O, T), R] =
    withParEffect(IO.fromFuture(IO(value)))

  @inline final def apply[G[_]: Source, T](source: G[T]): EmitterBuilder[T, R] = asLatest(source)

  @inline final def asLatest[F[_]: Source, T](latest: F[T]): EmitterBuilder[T, R] =
    transform[T](source => source.withLatestMap(Observable.lift(latest))((_, u) => u))

  def withLatest[F[_]: Source, T](latest: F[T]): EmitterBuilder[(O, T), R] =
    transform[(O, T)](source => source.withLatest(Observable.lift(latest)))

  @inline final def asHead[F[_]: Source, T](source: F[T]): EmitterBuilder[T, R] = asEffect(
    Observable.lift(source).headIO,
  )
  @inline final def withHead[F[_]: Source, T](source: F[T]): EmitterBuilder[(O, T), R] = withEffect(
    Observable.lift(source).headIO,
  )

  @inline final def asSyncLatest[F[_]: Source, T](source: F[T]): EmitterBuilder[T, R] =
    asEffect(Observable.lift(source).syncLatestSyncIO).mapFilter(identity)
  @inline final def withSyncLatest[F[_]: Source, T](source: F[T]): EmitterBuilder[(O, T), R] =
    withEffect(Observable.lift(source).syncLatestSyncIO).mapFilter { case (o, t) => t.map(o -> _) }

  def scan[T](seed: T)(f: (T, O) => T): EmitterBuilder[T, R] =
    transform[T](source => source.scan(seed)(f))

  @inline final def asScan[T](seed: T)(f: T => T): EmitterBuilder[T, R] = scan(seed)((t, _) => f(t))

  def scan0[T](seed: T)(f: (T, O) => T): EmitterBuilder[T, R] =
    transform[T](source => source.scan0(seed)(f))

  @inline final def asScan0[T](seed: T)(f: T => T): EmitterBuilder[T, R] = scan0(seed)((t, _) => f(t))

  def debounce(duration: FiniteDuration): EmitterBuilder[O, R] =
    transform[O](source => source.debounce(duration))

  def debounceMillis(millis: Int): EmitterBuilder[O, R] =
    transform[O](source => source.debounceMillis(millis))

  def async: EmitterBuilder[O, R] = asyncMacro

  def asyncMacro: EmitterBuilder[O, R] =
    transform[O](source => source.asyncMacro)

  def asyncMicro: EmitterBuilder[O, R] =
    transform[O](source => source.asyncMicro)

  def delay(duration: FiniteDuration): EmitterBuilder[O, R] =
    transform[O](source => source.delay(duration))

  def delayMillis(millis: Int): EmitterBuilder[O, R] =
    transform[O](source => source.delayMillis(millis))

  def mapEffect[G[_]: RunEffect, T](f: O => G[T]): EmitterBuilder[T, R] =
    transform[T](source => source.mapEffect(f))

  def mapFuture[T](f: O => Future[T]): EmitterBuilder[T, R] = transform[T](source => source.mapFuture(f))

  def singleMapFuture[T](f: O => Future[T]): EmitterBuilder[T, R] =
    transform[T](source => source.singleMapFuture(f))

  def singleMapEffect[G[_]: RunEffect, T](f: O => G[T]): EmitterBuilder[T, R] =
    transform[T](source => source.singleMapEffect(f))

  def switchMapFuture[T](f: O => Future[T]): EmitterBuilder[T, R] =
    transform[T](source => source.switchMapFuture(f))

  def switchMapEffect[G[_]: RunEffect, T](f: O => G[T]): EmitterBuilder[T, R] =
    transform[T](source => source.switchMapEffect(f))

  def parMapFuture[T](f: O => Future[T]): EmitterBuilder[T, R] =
    transform[T](source => source.parMapFuture(f))

  def parMapEffect[G[_]: RunEffect, T](f: O => G[T]): EmitterBuilder[T, R] =
    transform[T](source => source.parMapEffect(f))

  @inline def mapResult[S](f: R => S): EmitterBuilder[O, S] =
    new EmitterBuilder.MapResult[O, R, S](this, f)
}

object EmitterBuilder {

  @inline final class MapResult[+O, +I, +R](base: EmitterBuilder[O, I], mapF: I => R) extends EmitterBuilder[O, R] {
    @inline def transformSink[T](f: Observer[T] => Observer[O]): EmitterBuilder[T, R] =
      new MapResult(base.transformSink(f), mapF)
    @inline def transform[T](f: Observable[O] => Observable[T]): EmitterBuilder[T, R] =
      new MapResult(base.transform(f), mapF)
    @inline def forwardTo[F[_]: Sink, O2 >: O](sink: F[O2]): R = mapF(base.forwardTo(sink))
  }

  @inline final class Empty[+R](empty: R) extends EmitterBuilder[Nothing, R] {
    @inline def transformSink[T](f: Observer[T] => Observer[Nothing]): EmitterBuilder[T, R] = this
    @inline def transform[T](f: Observable[Nothing] => Observable[T]): EmitterBuilder[T, R] = this
    @inline def forwardTo[F[_]: Sink, O2 >: Nothing](sink: F[O2]): R                        = empty
  }

  @inline final class Stream[+O, +R: SubscriptionOwner: SyncEmbed](source: Observable[O], result: R)
      extends EmitterBuilder[O, R] {
    @inline def transformSink[T](f: Observer[T] => Observer[O]): EmitterBuilder[T, R] =
      new Stream(source.transformSink(f), result)
    @inline def transform[T](f: Observable[O] => Observable[T]): EmitterBuilder[T, R] = new Stream(f(source), result)
    @inline def forwardTo[F[_]: Sink, O2 >: O](sink: F[O2]): R =
      SubscriptionOwner[R].own(result)(() => source.unsafeSubscribe(Observer.lift(sink)))
  }

  @inline final class Custom[+O, +R: SubscriptionOwner: SyncEmbed](create: Observer[O] => R)
      extends EmitterBuilder[O, R] {
    @inline def transformSink[T](f: Observer[T] => Observer[O]): EmitterBuilder[T, R] = new TransformSink(this, f)
    @inline def transform[T](f: Observable[O] => Observable[T]): EmitterBuilder[T, R] = new Transform(this, f)
    @inline def forwardTo[F[_]: Sink, O2 >: O](sink: F[O2]): R                        = create(Observer.lift(sink))
  }

  @inline final class TransformSink[+I, +O, +R: SubscriptionOwner: SyncEmbed](
    base: EmitterBuilder[I, R],
    transformF: Observer[O] => Observer[I],
  ) extends EmitterBuilder[O, R] {
    @inline def transformSink[T](f: Observer[T] => Observer[O]): EmitterBuilder[T, R] =
      new TransformSink(base, s => transformF(f(s)))
    @inline def transform[T](f: Observable[O] => Observable[T]): EmitterBuilder[T, R] =
      new Transform[I, T, R](base, s => f(s.transformSink(transformF)))
    @inline def forwardTo[F[_]: Sink, O2 >: O](sink: F[O2]): R = base.forwardTo(transformF(Observer.lift(sink)))
  }

  @inline final class Transform[+I, +O, +R: SubscriptionOwner: SyncEmbed](
    base: EmitterBuilder[I, R],
    transformF: Observable[I] => Observable[O],
  ) extends EmitterBuilder[O, R] {
    @inline def transformSink[T](f: Observer[T] => Observer[O]): EmitterBuilder[T, R] =
      new Transform[I, T, R](base, s => transformF(s).transformSink(f))
    @inline def transform[T](f: Observable[O] => Observable[T]): EmitterBuilder[T, R] =
      new Transform[I, T, R](base, s => f(transformF(s)))
    @inline def forwardTo[F[_]: Sink, O2 >: O](sink: F[O2]): R = forwardToInTransform(base, transformF, sink)
  }

  // TODO: we requiring Monoid here, but actually just want an empty. Would allycats be better with Empty?
  @inline def emptyOf[R: Monoid]: EmitterBuilder[Nothing, R] = new Empty[R](Monoid[R].empty)

  @inline def apply[E, R: SubscriptionOwner: SyncEmbed](create: Observer[E] => R): EmitterBuilder[E, R] =
    new Custom[E, R](sink => create(sink))

  @inline def fromSourceOf[F[_]: Source, E, R: SubscriptionOwner: SyncEmbed: Monoid](
    source: F[E],
  ): EmitterBuilder[E, R] = new Stream[E, R](Observable.lift(source), Monoid[R].empty)

  // shortcuts for modifiers with less type ascriptions
  @inline def empty: EmitterBuilder[Nothing, VMod] = emptyOf[VMod]
  @inline def ofModifier[E](create: Observer[E] => VMod): EmitterBuilder[E, VMod] =
    apply[E, VMod](create)
  @inline def ofNode[E](create: Observer[E] => VNode): EmitterBuilder[E, VNode] = apply[E, VNode](create)
  @inline def fromSource[F[_]: Source, E](source: F[E]): EmitterBuilder[E, VMod] =
    fromSourceOf[F, E, VMod](source)

  def fromEvent[E <: dom.Event](eventType: String): EmitterBuilder[E, VMod] = apply[E, VMod] { sink =>
    Emitter(eventType, e => sink.unsafeOnNext(e.asInstanceOf[E]))
  }

  @inline def merge[T, R: SubscriptionOwner: SyncEmbed: Monoid](
    builders: EmitterBuilder[T, R]*,
  ): EmitterBuilder[T, R] = mergeIterable(builders)

  def mergeIterable[T, R: SubscriptionOwner: SyncEmbed: Monoid](
    builders: Iterable[EmitterBuilder[T, R]],
  ): EmitterBuilder[T, R] = new Custom[T, R](sink => Monoid[R].combineAll(builders.map(_.forwardTo(sink))))

  implicit def monoid[T, R: SubscriptionOwner: SyncEmbed: Monoid]: Monoid[EmitterBuilder[T, R]] =
    new Monoid[EmitterBuilder[T, R]] {
      def empty: EmitterBuilder[T, R] = EmitterBuilder.emptyOf[R]
      def combine(
        x: EmitterBuilder[T, R],
        y: EmitterBuilder[T, R],
      ): EmitterBuilder[T, R] = EmitterBuilder.merge(x, y)
    }

  implicit def functor[R]: Functor[EmitterBuilder[*, R]] = new Functor[EmitterBuilder[*, R]] {
    def map[A, B](fa: EmitterBuilder[A, R])(f: A => B): EmitterBuilder[B, R] = fa.map(f)
  }

  implicit object bifunctor extends Bifunctor[EmitterBuilder] {
    def bimap[A, B, C, D](fab: EmitterBuilder[A, B])(f: A => C, g: B => D): EmitterBuilder[C, D] =
      fab.map(f).mapResult(g)
  }

  @inline implicit class HandlerIntegrationMonoid[O, R: Monoid](
    builder: EmitterBuilder[O, R],
  ) {
    @inline def handled(f: Observable[O] => R): SyncIO[R] = handledF[SyncIO](f)

    @inline def handledF[F[_]: Sync](f: Observable[O] => R): F[R] =
      Functor[F].map(Sync[F].delay(Subject.replayLatest[O]())) { handler =>
        Monoid[R].combine(builder.forwardTo(handler), f(handler))
      }
  }

  @inline implicit class HandlerIntegration[O, R](builder: EmitterBuilder[O, R]) {
    @inline def handledWith(f: (R, Observable[O]) => R): SyncIO[R] = handledWithF[SyncIO](f)

    @inline def handledWithF[F[_]: Sync](f: (R, Observable[O]) => R): F[R] =
      Functor[F].map(Sync[F].delay(Subject.replayLatest[O]())) { handler =>
        f(builder.forwardTo(handler), handler)
      }
  }

  @inline implicit final class VModEventOperations(
    val builder: EmitterBuilder[VMod, VMod],
  ) extends AnyVal {
    @inline def render: VMod = builder.handled(VMod(_))
  }

  @inline implicit class EmitterOperations[O, R: Monoid: SubscriptionOwner: SyncEmbed](
    builder: EmitterBuilder[O, R],
  ) {

    @inline def withLatestEmitter[T](emitter: EmitterBuilder[T, R]): EmitterBuilder[(O, T), SyncIO[R]] =
      combineWithLatestEmitter(builder, emitter)
    @inline def asLatestEmitter[T](emitter: EmitterBuilder[T, R]): EmitterBuilder[T, SyncIO[R]] =
      withLatestEmitter(emitter).map(_._2)

    @inline def merge[T >: O](emitter: EmitterBuilder[T, R]): EmitterBuilder[T, R] =
      EmitterBuilder.merge(builder, emitter)
  }

  @inline implicit class EventActions[O <: dom.Event, R](val builder: EmitterBuilder[O, R]) extends AnyVal {
    def onlyOwnEvents: EmitterBuilder[O, R]   = builder.filter(ev => ev.currentTarget == ev.target)
    def preventDefault: EmitterBuilder[O, R]  = builder.map { e => e.preventDefault(); e }
    def stopPropagation: EmitterBuilder[O, R] = builder.map { e => e.stopPropagation(); e }
  }

  @inline implicit class TargetAsInput[O <: dom.Event, R](builder: EmitterBuilder[O, R]) {
    object target {
      @inline def value: EmitterBuilder[String, R] = builder.map(_.target.asInstanceOf[dom.html.Input].value)
      @inline def valueAsNumber: EmitterBuilder[Double, R] =
        builder.map(_.target.asInstanceOf[dom.html.Input].valueAsNumber)
      @inline def checked: EmitterBuilder[Boolean, R] = builder.map(_.target.asInstanceOf[dom.html.Input].checked)
      @inline def asHtml: EmitterBuilder[dom.html.Element, R] =
        builder.map(_.target.asInstanceOf[dom.html.Element])
      @inline def asSvg: EmitterBuilder[dom.svg.Element, R] = builder.map(_.target.asInstanceOf[dom.svg.Element])
      @inline def asElement: EmitterBuilder[dom.Element, R] = builder.map(_.target.asInstanceOf[dom.Element])
    }
  }

  @inline implicit class CurrentTargetOnEvent[O <: dom.Event, R](val builder: EmitterBuilder[O, R]) extends AnyVal {
    @inline def value: EmitterBuilder[String, R] = builder.map(_.currentTarget.asInstanceOf[dom.html.Input].value)
    @inline def valueAsNumber: EmitterBuilder[Double, R] =
      builder.map(_.currentTarget.asInstanceOf[dom.html.Input].valueAsNumber)
    @inline def checked: EmitterBuilder[Boolean, R] =
      builder.map(_.currentTarget.asInstanceOf[dom.html.Input].checked)
    @inline def asHtml: EmitterBuilder[dom.html.Element, R] =
      builder.map(_.currentTarget.asInstanceOf[dom.html.Element])
    @inline def asSvg: EmitterBuilder[dom.svg.Element, R] =
      builder.map(_.currentTarget.asInstanceOf[dom.svg.Element])
    @inline def asElement: EmitterBuilder[dom.Element, R] = builder.map(_.currentTarget.asInstanceOf[dom.Element])
  }

  @inline implicit class TypedElements[O <: dom.Element, R](val builder: EmitterBuilder[O, R]) extends AnyVal {
    @inline def asHtml: EmitterBuilder[dom.html.Element, R] =
      builder.asInstanceOf[EmitterBuilder[dom.html.Element, R]]
    @inline def asSvg: EmitterBuilder[dom.svg.Element, R] =
      builder.asInstanceOf[EmitterBuilder[dom.svg.Element, R]]
  }

  @inline implicit class TypedElementTuples[E <: dom.Element, R](val builder: EmitterBuilder[(E, E), R])
      extends AnyVal {
    @inline def asHtml: EmitterBuilder[(dom.html.Element, dom.html.Element), R] =
      builder.asInstanceOf[EmitterBuilder[(dom.html.Element, dom.html.Element), R]]
    @inline def asSvg: EmitterBuilder[(dom.svg.Element, dom.svg.Element), R] =
      builder.asInstanceOf[EmitterBuilder[(dom.svg.Element, dom.svg.Element), R]]
  }

  @noinline private def combineWithLatestEmitter[O, T, R: Monoid: SubscriptionOwner](
    sourceEmitter: EmitterBuilder[O, R],
    latestEmitter: EmitterBuilder[T, R],
  ): EmitterBuilder[(O, T), SyncIO[R]] =
    new Custom[(O, T), SyncIO[R]]({ sink =>
      SyncIO {
        var lastValue: Option[T] = None
        Monoid[R].combine(
          latestEmitter.forwardTo(Observer.create[T](v => lastValue = Some(v), sink.unsafeOnError)),
          sourceEmitter.forwardTo(
            Observer.create[O](
              { o =>
                lastValue.foreach { t =>
                  sink.unsafeOnNext((o, t))
                }
              },
              sink.unsafeOnError,
            ),
          ),
        )
      }
    })

  @noinline private def forwardToInTransform[F[_]: Sink, I, O, O2 >: O, R: SubscriptionOwner: SyncEmbed](
    base: EmitterBuilder[I, R],
    transformF: Observable[I] => Observable[O],
    sink: F[O2],
  ): R = SyncEmbed[R].delay {
    val connectable = Observer.lift(sink).redirect(transformF)
    SubscriptionOwner[R].own(base.forwardTo(connectable.value))(connectable.unsafeConnect)
  }
}

trait EventDispatcher[-T] {
  def dispatch(source: Observable[T]): Observable[Any]
}
object EventDispatcher {
  def ofModelUpdate[M, T](subject: Subject[M], update: (T, M) => M) = new EventDispatcher[T] {
    def dispatch(source: Observable[T]) = source.withLatestMap(subject)(update).via(subject)
  }
}
