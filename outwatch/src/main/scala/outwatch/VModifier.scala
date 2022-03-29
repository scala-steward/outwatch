package outwatch

import outwatch.helpers.{BasicAttrBuilder, PropBuilder, BasicStyleBuilder, ModifierBooleanOps}
import outwatch.helpers.NativeHelpers._
import snabbdom.{DataObject, VNodeProxy}

import colibri._
import colibri.effect._
import cats.Monoid
import cats.syntax.either._
import cats.syntax.functor._
import cats.effect.Sync

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

sealed trait VModifier

object VModifier {
  @inline def empty: VModifier = EmptyModifier

  @inline def apply(): VModifier = empty

  @inline def apply[T : Render](t: T): VModifier = Render[T].render(t)

  @inline def apply(modifier: VModifier, modifier2: VModifier): VModifier =
    CompositeModifier(js.Array(modifier, modifier2))

  @inline def apply(modifier: VModifier, modifier2: VModifier, modifier3: VModifier): VModifier =
    CompositeModifier(js.Array(modifier, modifier2, modifier3))
  @inline final def attr[T](key: String, convert: T => Attr.Value = (t: T) => t.toString : Attr.Value) = new BasicAttrBuilder[T](key, convert)
  @inline final def prop[T](key: String, convert: T => Prop.Value = (t: T) => t) = new PropBuilder[T](key, convert)
  @inline final def style[T](key: String) = new BasicStyleBuilder[T](key)

  @inline def managed[F[_] : Sync : RunEffect, T : CanCancel](subscription: F[T]): VModifier = VModifier(
    subscription.map[VModifier](cancelable => CancelableModifier(() => Cancelable.lift(cancelable)))
  )

  @inline def managedSubscribe[F[_] : Source, T](source: F[T]): VModifier = managedDelay(Source[F].unsafeSubscribe(source)(Observer.empty))

  @deprecated("Use managedDelay(subscription) instead", "")
  @inline def managedFunction[T : CanCancel](subscription: () => T): VModifier = managedDelay(subscription())
  @inline def managedDelay[T : CanCancel](subscription: => T): VModifier = CancelableModifier(() => Cancelable.lift(subscription))

  object managedElement {
    def apply[T : CanCancel](subscription: dom.Element => T): VModifier = VModifier.delay {
      var lastSub: js.UndefOr[T] = js.undefined
      VModifier(
        DomMountHook(proxy => proxy.elm.foreach(elm => lastSub = subscription(elm))),
        DomUnmountHook(_ => lastSub.foreach(CanCancel[T].unsafeCancel))
      )
    }

    @inline def asHtml[T : CanCancel](subscription: dom.html.Element => T): VModifier = apply(elem => subscription(elem.asInstanceOf[dom.html.Element]))

    @inline def asSvg[T : CanCancel](subscription: dom.svg.Element => T): VModifier = apply(elem => subscription(elem.asInstanceOf[dom.svg.Element]))
  }

  @inline def apply(modifier: VModifier, modifier2: VModifier, modifier3: VModifier, modifier4: VModifier): VModifier =
    CompositeModifier(js.Array(modifier, modifier2, modifier3, modifier4))

  @inline def apply(modifier: VModifier, modifier2: VModifier, modifier3: VModifier, modifier4: VModifier, modifier5: VModifier): VModifier =
    CompositeModifier(js.Array(modifier, modifier2, modifier3, modifier4, modifier5))

  @inline def apply(modifier: VModifier, modifier2: VModifier, modifier3: VModifier, modifier4: VModifier, modifier5: VModifier, modifier6: VModifier): VModifier =
    CompositeModifier(js.Array(modifier, modifier2, modifier3, modifier4, modifier5, modifier6))

  @inline def apply(modifier: VModifier, modifier2: VModifier, modifier3: VModifier, modifier4: VModifier, modifier5: VModifier, modifier6: VModifier, modifier7: VModifier, modifiers: VModifier*): VModifier =
    CompositeModifier(js.Array(modifier, modifier2, modifier3, modifier4, modifier5, modifier6, modifier7, CompositeModifier(modifiers)))

  @inline def fromEither[T : Render](modifier: Either[Throwable, T]): VModifier = modifier.fold(raiseError(_), apply(_))
  @inline def delayEither[T : Render](modifier: => Either[Throwable, T]): VModifier = SyncEffectModifier(() => fromEither(modifier))
  @inline def delay[T : Render](modifier: => T): VModifier = delayEither(Either.catchNonFatal(modifier))
  @inline def composite(modifiers: Iterable[VModifier]): VModifier = CompositeModifier(modifiers.toJSArray)
  @inline def raiseError[T](error: Throwable): VModifier = ErrorModifier(error)

  @inline def ifTrue(condition: Boolean): ModifierBooleanOps = new ModifierBooleanOps(condition)
  @inline def ifNot(condition: Boolean): ModifierBooleanOps = new ModifierBooleanOps(!condition)

  implicit object monoid extends Monoid[VModifier] {
    @inline def empty: VModifier = VModifier.empty
    @inline def combine(x: VModifier, y: VModifier): VModifier = VModifier(x, y)
  }

  implicit object subscriptionOwner extends SubscriptionOwner[VModifier] {
    @inline def own(owner: VModifier)(subscription: () => Cancelable): VModifier = VModifier(managedDelay(subscription()), owner)
  }

  @inline implicit def renderToVModifier[T : Render](value: T): VModifier = Render[T].render(value)
  }

sealed trait StaticVModifier extends VModifier

final case class VNodeProxyNode(proxy: VNodeProxy) extends StaticVModifier

final case class Key(value: Key.Value) extends StaticVModifier
object Key {
  type Value = DataObject.KeyValue
}

final case class Emitter(eventType: String, trigger: js.Function1[dom.Event, Unit]) extends StaticVModifier

sealed trait Attr extends StaticVModifier
object Attr {
  type Value = DataObject.AttrValue
}
final case class BasicAttr(title: String, value: Attr.Value) extends Attr
final case class AccumAttr(title: String, value: Attr.Value, accum: (Attr.Value, Attr.Value)=> Attr.Value) extends Attr

final case class Prop(title: String, value: Prop.Value) extends StaticVModifier
object Prop {
  type Value = DataObject.PropValue
}

sealed trait Style extends StaticVModifier
final case class AccumStyle(title: String, value: String, accum: (String, String) => String) extends Style
final case class BasicStyle(title: String, value: String) extends Style
final case class DelayedStyle(title: String, value: String) extends Style
final case class RemoveStyle(title: String, value: String) extends Style
final case class DestroyStyle(title: String, value: String) extends Style

sealed trait SnabbdomHook extends StaticVModifier
final case class InitHook(trigger: js.Function1[VNodeProxy, Unit]) extends SnabbdomHook
final case class InsertHook(trigger: js.Function1[VNodeProxy, Unit]) extends SnabbdomHook
final case class PrePatchHook(trigger: js.Function2[VNodeProxy, VNodeProxy, Unit]) extends SnabbdomHook
final case class UpdateHook(trigger: js.Function2[VNodeProxy, VNodeProxy, Unit]) extends SnabbdomHook
final case class PostPatchHook(trigger: js.Function2[VNodeProxy, VNodeProxy, Unit]) extends SnabbdomHook
final case class DestroyHook(trigger: js.Function1[VNodeProxy, Unit]) extends SnabbdomHook

sealed trait DomHook extends StaticVModifier
final case class DomMountHook(trigger: js.Function1[VNodeProxy, Unit]) extends DomHook
final case class DomUnmountHook(trigger: js.Function1[VNodeProxy, Unit]) extends DomHook
final case class DomUpdateHook(trigger: js.Function2[VNodeProxy, VNodeProxy, Unit]) extends DomHook
final case class DomPreUpdateHook(trigger: js.Function2[VNodeProxy, VNodeProxy, Unit]) extends DomHook

final case class NextVModifier(modifier: StaticVModifier) extends StaticVModifier

case object EmptyModifier extends VModifier
final case class CompositeModifier(modifiers: Iterable[VModifier]) extends VModifier
final case class StreamModifier(subscription: Observer[VModifier] => Cancelable) extends VModifier
final case class ChildCommandsModifier(commands: Observable[Seq[ChildCommand]]) extends VModifier
final case class CancelableModifier(subscription: () => Cancelable) extends VModifier
final case class SyncEffectModifier(unsafeRun: () => VModifier) extends VModifier
final case class ErrorModifier(error: Throwable) extends VModifier
final case class StringVNode(text: String) extends VModifier

sealed trait VNode extends VModifier {
  def apply(args: VModifier*): VNode
  def append(args: VModifier*): VNode
  def prepend(args: VModifier*): VNode
}
object VNode {
  @inline final def html(name: String): HtmlVNode = HtmlVNode(name, js.Array[VModifier]())
  @inline final def svg(name: String): SvgVNode = SvgVNode(name, js.Array[VModifier]())

  implicit object subscriptionOwner extends SubscriptionOwner[VNode] {
    @inline def own(owner: VNode)(subscription: () => Cancelable): VNode = owner.append(VModifier.managedDelay(subscription()))
  }
}
sealed trait BasicVNode extends VNode {
  def nodeType: String
  def modifiers: js.Array[VModifier]
  def apply(args: VModifier*): BasicVNode
  def append(args: VModifier*): BasicVNode
  def prepend(args: VModifier*): BasicVNode
  def thunk(key: Key.Value)(arguments: Any*)(renderFn: => VModifier): ThunkVNode = ThunkVNode(this, key, arguments.toJSArray, () => renderFn)
  def thunkConditional(key: Key.Value)(shouldRender: Boolean)(renderFn: => VModifier): ConditionalVNode = ConditionalVNode(this, key, shouldRender, () => renderFn)
  @inline def thunkStatic(key: Key.Value)(renderFn: => VModifier): ConditionalVNode = thunkConditional(key)(false)(renderFn)
}
@inline final case class ThunkVNode(baseNode: BasicVNode, key: Key.Value, arguments: js.Array[Any], renderFn: () => VModifier) extends VNode {
  @inline def apply(args: VModifier*): ThunkVNode = append(args: _*)
  def append(args: VModifier*): ThunkVNode = copy(baseNode = baseNode(args: _*))
  def prepend(args: VModifier*): ThunkVNode = copy(baseNode = baseNode.prepend(args :_*))
}
@inline final case class ConditionalVNode(baseNode: BasicVNode, key: Key.Value, shouldRender: Boolean, renderFn: () => VModifier) extends VNode {
  @inline def apply(args: VModifier*): ConditionalVNode = append(args: _*)
  def append(args: VModifier*): ConditionalVNode = copy(baseNode = baseNode(args: _*))
  def prepend(args: VModifier*): ConditionalVNode = copy(baseNode = baseNode.prepend(args: _*))
}
@inline final case class HtmlVNode(nodeType: String, modifiers: js.Array[VModifier]) extends BasicVNode {
  @inline def apply(args: VModifier*): HtmlVNode = append(args: _*)
  def append(args: VModifier*): HtmlVNode = copy(modifiers = appendSeq(modifiers, args))
  def prepend(args: VModifier*): HtmlVNode = copy(modifiers = prependSeq(modifiers, args))
}
@inline final case class SvgVNode(nodeType: String, modifiers: js.Array[VModifier]) extends BasicVNode {
  @inline def apply(args: VModifier*): SvgVNode = append(args: _*)
  def append(args: VModifier*): SvgVNode = copy(modifiers = appendSeq(modifiers, args))
  def prepend(args: VModifier*): SvgVNode = copy(modifiers = prependSeq(modifiers, args))
}
