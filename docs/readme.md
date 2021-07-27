
Welcome to Outwatch!



## Getting started

### Start with a template
For a quick start, install `java`, `sbt`, `nodejs` and `yarn` and use the following g8 template:
```bash
sbt new outwatch/seed.g8
```

In your newly created project folder, run:
```bash
sbt dev
```

Now point your browser to http://localhost:8080.

Changes to the code will trigger a recompile and automatically refresh the page in the browser.


### Create a project from scratch
Install `java`, `sbt`, `nodejs` and `yarn`, if you haven't already.
Create a new SBT project and add the `scalajs` and `scalajs-bundler` plugins to your `project/plugins.sbt`:
```scala
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.6.0")
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.20.0")
```

Add the outwatch dependencies to your `build.sbt`:
```scala
resolvers += "jitpack" at "https://jitpack.io"
val outwatchVersion = "@HEADCOMMIT@"
libraryDependencies ++= Seq(
  "com.github.outwatch.outwatch" %%% "outwatch"      % outwatchVersion,
  // optional dependencies:
  "com.github.cornerman.colibri" %%% "colibri-monix" % outwatchVersion, // Monix
  "com.github.cornerman.colibri" %%% "colibri-rx"    % outwatchVersion, // Scala.rx
  "com.github.outwatch.outwatch" %%% "outwatch-util" % outwatchVersion, // Store, Websocket, Http
)

```

Enable the `scalajs-bundler` plugin:
```scala
enablePlugins(ScalaJSPlugin)
enablePlugins(ScalaJSBundlerPlugin)
```

To configure hot reloading with webpack devserver, check out [build.sbt](https://github.com/OutWatch/seed.g8/blob/master/src/main/g8/build.sbt) and [webpack.config.dev.js](https://github.com/OutWatch/seed.g8/blob/master/src/main/g8/webpack.config.dev.js) from the [g8 template](https://github.com/OutWatch/seed.g8).

If anything is not working, cross-check how things are done in the template.

We're using [JitPack](https://jitpack.io) to release the libraries. With JitPack you can easily try the latest features from specific commits on `master`, other branches or PRs. Just point `outwatchVersion` to a specific commit.

### Use common javascript libraries with OutWatch

We have prepared helpers for some javascript libraries. You can find them in the [OutWatch-libs](https://github.com/outwatch/outwatch-libs) Repository.

## Examples


## Hello World
In your html file, create an element, which you want to replace with dynamic content:

```html
...
<body>
  <div id="app"></div>
  <!-- your compiled javascript should be imported here -->
</body>
...
```

To render html content with outwatch, create a component and render it into the given element:

```scala mdoc:js:compile-only
import outwatch._
import outwatch.dsl._
import cats.effect.IO


object Main {
  def main(args: Array[String]): Unit = {
    
    val myComponent = div("Hello World")

    OutWatch.renderReplace[IO]("#app", myComponent).unsafeRunSync()
  }
}
```

Running `Main` will replace `<div id="app"></div>` with `myComponent`:

```html
...
<body>
  <div id="app">Hello World</div>
  ...
</body>
...
```

## Interactive Counter

```scala mdoc:js
import outwatch._
import outwatch.dsl._
import colibri._
import cats.effect.IO

val component = {
  val counter = Subject.behavior(0)
  div(
    button("+", onClick(counter.map(_ + 1)) --> counter),
    counter,
  )
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```

To understand how this example works in-depth, please read about [Dynamic Content](#dynamic-content) and [Handling Events](#handling-events).

**Important:** In your application, `OutWatch.renderReplace` should be called only once at the end of the main method. To create dynamic content, you will design your data-flow with `Obvervable`, `Subject` and/or `Scala.Rx` and then instantiate outwatch only once with this method call. Before that, no reactive subscriptions will happen.

## Static Content
First, we will focus on creating immutable/static content that will not change over time. The following examples illustrate to construct and transform HTML/SVG tags, attributes and inline stylesheets.

### Imports
```scala mdoc:js:shared
import outwatch._
import outwatch.dsl._
import cats.effect.IO
```

### Concatenating Strings
```scala mdoc:js:compile-only
val component = div("Hello ", "World")
// <div>Hello World</div>
```


### Nesting
```scala mdoc:js:compile-only
div(span("Hey ", b("you"), "!"))
// <div><span>Hey <b>you</b>!</span></div>
```


### Primitives
```scala mdoc:js:compile-only
div(true, 0, 1000L, 3.0)
// <div>true010003.0</div>
```


### Attributes

```scala mdoc:js:compile-only
div(idAttr := "test")
// <div id="test"></div>
```

The order of content and attributes does not matter.

```scala mdoc:js:compile-only
div("How ", idAttr := "test", "are", title := "cool", " you?")
// <div id="test" title="cool">How are you?</div>
```


### Styles
All style properties have to be written in *camelCase*.

```scala mdoc:js:compile-only
div(color := "tomato", "Hello")
// <div style="color: tomato">Hello</div>
```

Multiple styles will me merged to one style attribute:

```scala mdoc:js:compile-only
div(backgroundColor := "powderblue", border := "2px solid #222", "Hello")
// <div style="background-color: powderblue; border: 2px solid #222">Hello</div>
```

Again, the order of styles, attributes and inner tags does not matter:

```scala mdoc:js:compile-only
div(h1("Welcome to my website"), backgroundColor := "powderblue", idAttr := "header")
// <div style="background-color: powderblue" id="header">Welcome to my website</div>
```

Some styles have type safe values:

```scala mdoc:js:compile-only
div(cursor.pointer, fontWeight.bold, display.flex)
// <div style="cursor: pointer; font-weight: bold; display: flex;"></div>
```

If you are missing more type safe values, please contribute to [Scala Dom Types](https://github.com/raquo/scala-dom-types). Example implementation: [fontWeight](https://github.com/raquo/scala-dom-types/blob/master/shared/src/main/scala/com/raquo/domtypes/generic/defs/styles/Styles.scala#L1711)


### Reserved Scala keywords: class, for, type
There are some attributes and styles which are reserved scala keywords. You can use them with backticks:

```scala mdoc:js:compile-only
div(`class` := "item", "My Item")
// <div class="item">My Item</div>

label(`for` := "inputid")
// <label for="inputid" />

input(`type` := "text")
// <input type="text" />
```

There are shortcuts for the class and type atrributes:

```scala mdoc:js:compile-only
div(cls := "myclass")
// <div class="myclass"></div>

input(tpe := "text")
// <input type="text" />
```


### Overriding attributes
Attributes and styles with the same name will be overwritten. Last wins.

```scala mdoc:js:compile-only
div(color := "blue", color := "green")
// <div style="color: green"></div>
```

### CSS class accumulation
Classes are not overwritten, they accumulate.

```scala mdoc:js:compile-only
div(cls := "tiny", cls := "button")
// <div class="tiny button"></div>
```

### Custom attributes, styles and tags
All the tags, attributes and styles available in outwatch come from [Scala Dom Types](https://github.com/raquo/scala-dom-types).
If you want to use something not available in Scala Dom Types, you can use custom builders:

```scala mdoc:js:compile-only
htmlTag("app")(style("user-select") := "none", attr("everything") := "possible")
// <app style="user-select: none" everything="possible"></div>
```

You can also define the accumulation behavior of custom attributes:
```scala mdoc:js:compile-only
div(
  attr("everything").accum("-") := "is",
  attr("everything").accum("-") := "possible",
)
// <div everything="is-possible"></div>
```

If you think there is something missing in Scala Dom Types, please open a PR or Issue. Usually it's just a few lines of code.

Source Code: [DomTypes.scala](@REPOURL@/outwatch/src/main/scala/outwatch/definitions/DomTypes.scala)


### Data and Aria attributes
Data and aria attributes make use of [`scala.Dynamic`](https://www.scala-lang.org/api/current/scala/Dynamic.html), so you can write things like:

```scala mdoc:js:compile-only
div(
  data.payload := "17",
  data.`consent-required` := "Are you sure?",
  data.message.success := "Message sent!",
  aria.hidden := "true",
)
// <div 
//    data-payload="17"
//    data-consent-required="Are you sure?"
//    data-message-success="Message sent!"
//    aria-hidden="true"
//    >
// </div>
```

Source Code: [OutwatchAttributes.scala](@REPOURL@/outwatch/src/main/scala/outwatch/definitions/OutwatchAttributes.scala#L75), [Builder.scala](@REPOURL@/outwatch/src/main/scala/outwatch/helpers/Builder.scala#L35)


### SVG
SVG tags and attributes are available via an extra import. Namespacing is automatically handled for you.

```scala mdoc:js
val component = {
  import svg._
  svg(
    height := "100px",
    viewBox := "0 0 471.701 471.701",
    g(
      path(d := """M433.601,67.001c-24.7-24.7-57.4-38.2-92.3-38.2s-67.7,13.6-92.4,38.3l-12.9,12.9l-13.1-13.1
        c-24.7-24.7-57.6-38.4-92.5-38.4c-34.8,0-67.6,13.6-92.2,38.2c-24.7,24.7-38.3,57.5-38.2,92.4c0,34.9,13.7,67.6,38.4,92.3
        l187.8,187.8c2.6,2.6,6.1,4,9.5,4c3.4,0,6.9-1.3,9.5-3.9l188.2-187.5c24.7-24.7,38.3-57.5,38.3-92.4
        C471.801,124.501,458.301,91.701,433.601,67.001z M414.401,232.701l-178.7,178l-178.3-178.3c-19.6-19.6-30.4-45.6-30.4-73.3
        s10.7-53.7,30.3-73.2c19.5-19.5,45.5-30.3,73.1-30.3c27.7,0,53.8,10.8,73.4,30.4l22.6,22.6c5.3,5.3,13.8,5.3,19.1,0l22.4-22.4
        c19.6-19.6,45.7-30.4,73.3-30.4c27.6,0,53.6,10.8,73.2,30.3c19.6,19.6,30.3,45.6,30.3,73.3
        C444.801,187.101,434.001,213.101,414.401,232.701z""", fill := "tomato")
    )
  )
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```

### VNode and VDomModifier
The important types we are using in the examples above are `VNode` and `VDomModifier`. `VNode` represents a node in the virtual dom, while and `VDomModifier` represents atrributes and styles and children of a node.

```scala mdoc:js:compile-only
val vnode: VNode = div()
val modifiers: List[VDomModifier] = List("Hello", idAttr := "main", color := "tomato", vnode)
```

Every `VNode` contains a sequence of `VDomModifier`. And a `VNode` is a `VDomModifier` itself.



### Grouping Modifiers
To make a set of modifiers reusable you can group them to become one `VDomModifier`.

```scala mdoc:js:shared
val bigFont = VDomModifier(fontSize := "40px", fontWeight.bold)
div("Argh!", bigFont)
// <div style="font-size: 40px; font-weight: bold;">Argh!</div>
```

If you want to reuse `bigFont`, but want to overwrite one of its properties, simply append the overwriting modifier. Here the latter `fontSize` will overwrite the one from `bigFont`:
```scala mdoc:js:compile-only
val bigFont2 = VDomModifier(bigFont, fontSize := "99px")
```

You can also use a `Seq[VDomModifier]` directly instead of using `apply` defined in the [VDomModifier](@REPOURL@/outwatch/src/main/scala/outwatch/package.scala) object.


### Components
Outwatch does not have the concept of a component itself. You can just pass `VNode`s and `VDomModifier`s around and build your own abstractions using functions. When we are talking about components in this documentation, we are usually referring to a `VNode` or a function returning a `VNode`.

```scala mdoc:js:compile-only
def fancyHeadLine(content: String) = h1(borderBottom := "1px dashed tomato", content)
fancyHeadLine("I like tomatoes.")
// <h1 style="border-bottom: 1px dashed tomato;">I like tomatoes.</h1>
```


### Transforming Components
Components are immutable, we can only modify them by creating a changed copy. Like you may know from Scalatags, you can call `.apply(...)` on any `VNode`, *append* more modifiers and get a new `VNode` with the applied changes back.

```scala mdoc:js:compile-only
val x = div("dog")
x(title := "the dog")
// <div title="the dog">dog</div>
```

This can be useful for reusing html snippets.

```scala mdoc:js:compile-only
val box = div(width := "100px", height := "100px")

div(
  box(backgroundColor := "powderblue"),
  box(backgroundColor := "mediumseagreen"),
)

// <div>
//  <div style="width: 100px; height: 100px; background-color: powderblue;"> </div>
//  <div style="width: 100px; height: 100px; background-color: mediumseagreen;"></div>
// </div>
```

Since modifiers are *appended*, they can overwrite existing ones. This is useful to adjust existing components to your needs.

```scala js:mdoc:compile-only
val box = div(width := "100px", height := "100px")
box(backgroundColor := "mediumseagreen", width := "200px")
// <div style="width: 200px; height: 100px; background-color: mediumseagreen;"></div>
```

You can also *prepend* modifiers. This can be useful to provide defaults retroactively.

```scala js:mdoc:compile-only
def withBorderIfNotProvided(vnode: VNode) = vnode.prepend(border := "3px solid coral")
div(
  withBorderIfNotProvided(div("hello", border := "7px solid moccasin")),
  withBorderIfNotProvided(div("hello")),
)
// <div>
//   <div style="border: 7px solid moccasin;">hello</div>
//   <div style="border: 3px solid coral;">hello</div>
// </div>
```

Source Code: [VDomModifier.scala](@REPOURL@/outwatch/src/main/scala/outwatch/VDomModifier.scala#L92)


### Example: Flexbox
When working with [Flexbox](https://css-tricks.com/snippets/css/a-guide-to-flexbox/), you can set styles for the **container** and **children**. With `VNode.apply()` you can have all flexbox-related styles in one place. The child-components don't have to know anything about flexbox, even though they get specific styles assigned.

```scala js:mdoc:compile-only
val itemA = div("A", backgroundColor := "mediumseagreen")
val itemB = div("B", backgroundColor := "tomato")

div(
  height := "100px",
  border := "1px solid black",

  display.flex,

  itemA(flexBasis := "50px"),
  itemB(alignSelf.center),
)
// <div style="height: 100px; border: 1px solid black; display: flex;">
//   <div style="background-color: mediumseagreen; flex-basis: 50px;">A</div>
//   <div style="background-color: tomato; align-self: center;">B</div>
// </div>
```


### Option and Seq

Outwatch can render anything that implements the type class [`Render`](@REPOURL@/outwatch/src/main/scala/outwatch/Render.scala). Instances for types like `Option` and `Seq` are built-in and can be arbitrarily combined: 

```scala mdoc:js:compile-only
div(
  Some("thing"),
  Some(color := "steelblue"),
  fontSize :=? Some("70px"),
  Seq("Hey", "How are you?"),
  List("a", "b", "c").map(span(_)),
  Some(Seq("x")),
)
// <div style="color: steelblue; font-size: 70px;">
//   thing
//   Hey
//   How are you?
//   <span>a</span>
//   <span>b</span>
//   <span>c</span>
//   x
// </div>
```

Note, that outwatch does not accept `Set`, since the order is undefined.


### Rendering Custom Types

You can render any custom type by implementing the typeclass `Render`:

```scala mdoc:js:compile-only
case class Person(name: String, age: Int)

// Type class instance for `Render`:
object Person {
  implicit object PersonRender extends Render[Person] {
    def render(person: Person): VDomModifier = div(
        b(person.name), span(person.age, marginLeft := "5px")
    )
  }
}
```

Now you can just use instances of `Person` in your dom definitions:
```scala js:mdoc:compile-only
val person = Person("Hans", age = 48)

div(person)
```

Source Code: [Render.scala](@REPOURL@/outwatch/src/main/scala/outwatch/Render.scala)

## Dynamic Content

### Reactive Programming

Outwatch natively renders reactive data types, like `Observable` or `Rx` (Scala.Rx). Outwatch internally uses the [`colibri`](https://github.com/cornerman/colibri) library and therefore provides lightweight observables and subjects out of the box. `BehaviorSubject` (which is also an `Observable`) is especially useful to deal with state. All subscriptions are automatically handled by outwatch.

```scala mdoc:js:shared
import outwatch._
import outwatch.dsl._
import cats.effect.IO
```

```scala mdoc:js
import concurrent.duration._

import monix.execution.Scheduler.Implicits.global
import colibri.ext.monix._

val component = {
  div(
    div(
      "Observable (colibri): ",
      colibri.Observable.interval(1.second),
    ),
    div(
      "Observable (monix): ",
      monix.reactive.Observable.interval(1.second),
    ),
  )
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```

**Important:** In your application, `OutWatch.renderReplace` should be called only once at the end of the main method. To create dynamic content, you will design your data-flow with `Obvervable`, `Subject` and/or `Scala.Rx` and then instantiate outwatch only once with this method call. Before that, no reactive subscriptions will happen.

### Reactive attributes
Attributes can also take dynamic values.

```scala mdoc:js
import colibri.Observable
import concurrent.duration._

val component = {
  val boxWidth = Observable.interval(1.second).map(i => if(i % 2 == 0) "100px" else "50px")
  div(
    width <-- boxWidth,
    height := "50px",
    backgroundColor := "cornflowerblue",
    transition := "width 0.5s",
  )
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```

### Reactive Modifiers and VNodes
You can stream any `VDomModifier` and therefore whole components, attributes, styles, sets of modifiers, and so on:

```scala mdoc:js
import colibri.Observable
import concurrent.duration._

val component = {
  div(
    div(
      width := "50px",
      height := "50px",
      Observable.interval(1.second)
        .map{ i => 
          val color = if(i % 2 == 0) "tomato" else "cornflowerblue"
          backgroundColor := color
        },
    ),
  )
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```



```scala mdoc:js
import colibri.Observable
import concurrent.duration._

val component = {
  val nodeStream: Observable[VNode] = Observable(div("I am delayed!")).delay(5.seconds)
  div("Hello ", nodeStream)
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```


### Rendering Futures

Futures are natively supported too:

```scala mdoc:js
import scala.concurrent.Future
implicit val ec = scala.concurrent.ExecutionContext.global

val component = {
  div(
    Future { 1 + 1 },
  )
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```

### Higher Order Reactiveness

Don't fear to nest different reactive constructs. Outwatch will handle everything for you. Example use-cases include sequences of api-requests, live database queries, etc.

```scala mdoc:js
import scala.concurrent.Future
implicit val ec = scala.concurrent.ExecutionContext.global
import concurrent.duration._
import colibri.Observable

val component = {
  div(
    div(Observable.interval(1.seconds).map(i => Future { i*i })),
    div(Observable.interval(1.seconds).map(i => IO { i*2 })),
    div(IO { Observable.interval(1.seconds) }),
  )
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```

### Using other streaming libraries

We use the library [`colibri`](https://github.com/cornerman/colibri) for a minimal reactive library and for typeclasses around streaming. These typeclasses like `Source` and `Sink` allow to integrate third-party libraries for streaming easily.

For using outwatch with monix:
```scala mdoc:js:compile-only
import colibri.ext.monix._
```

For using outwatch with scala.rx:
```scala mdoc:js:compile-only
import colibri.ext.rx._
```


### Dom Lifecycle Mangement

OutWatch automatically handles subscriptions of streams and observables in your components. You desribe your component with static and dynamic content (subjects, event emitters, `-->` and `<--`). When using these components, the needed subscriptions will be tied to the lifecycle of the respective dom elements and are managed for you. So whenever an element is mounted the subscriptions are run and whenever it is unmounted the subscriptions are killed.

If you ever need to manually subscribe to a stream, you can let OutWatch manage the subscription for you:

```scala
import cats.effect.SyncIO

div(
  managed(SyncIO { myObservable.subscribe(???) }) // this subscription is now bound to the lifetime of the outer div element
)
```



## Handling Events

Outwatch allows to react to dom events in a very flexible way. Every event handler emits events to be further processed. Events can trigger side-effects, be filtered, mapped, replaced or forwarded to reactive variables. In outwatch this concept is called [EmitterBuilder](@REPOURL@/outwatch/src/main/scala/outwatch/EmitterBuilder.scala). For example, take the `click` event for which `onClick` is an `EmitterBuilder`:

```scala mdoc:js
import org.scalajs.dom.console

val component = {
  div(
    button(
      "Log a Click-Event",
      onClick.foreach { e => console.log("Event: ", e) },
    ),
    " (Check the browser console to see the event)"
  )
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```

In the next example, we map the event `e` to extract `e.clientX` and write the result into the reactive variable (`BehaviorSubject`) called `x`:

```scala mdoc:js
import colibri.Subject
val component = {
  val x = Subject.behavior(0.0)
  div(
      div(
        "Hover to see mouse x-coordinate",
        onMouseMove.map(e => e.clientX) --> x,
        backgroundColor := "lightpink",
        cursor.crosshair,
      ),
      div(" x = ", x )
   )
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```

`EmitterBuilder` comes with many useful methods to make your life easier. Check the completions of your editor.

### Example: Counter

We don't have to use the dom-event at all. Often it's useful to replace it with a constant or the current value of a reactive variable. Let's revisit the counter example:

```scala mdoc:js
import colibri.Subject
val component = {
  val counter = Subject.behavior(0)
  div(
      button("increase", onClick(counter.map(_ + 1)) --> counter),
      button("decrease", onClick(counter.map(_ - 1)) --> counter),
      button("reset", onClick.use(0) --> counter),
      div("counter: ", counter )
   )
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```


### Example: Input field

Another common use-case is the handling of values of input fields. Here the `BehaviorSubject` called `text` reflects the current value of the input field. On every keystroke, `onInput` emits an event, `.value` extracts the string from the input field and writes it into `text`. But the `value` attribute of the input field also reacts on changes to the reactive variable, which we use to clear the text field. We have a second reactive variable that holds the submitted value after pressing enter.

```scala mdoc:js
import colibri.Subject
import org.scalajs.dom.ext.KeyCode

// Emitterbuilders can be extracted and reused!
val onEnter = onKeyDown
    .filter(e => e.keyCode == KeyCode.Enter)
    .preventDefault

val component = {
  val text = Subject.behavior("")
  val submitted = Subject.behavior("")
  div(
      input(
        tpe := "text",
        value <-- text,
        onInput.value --> text,
        onEnter(text) --> submitted,
      ),
      button("clear", onClick.use("") --> text),
      div("text: ", text),
      div("length: ", text.map(_.length)),
      div("submitted: ", submitted),
   )
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```

Another example with `debounce` functionality. The `debounced` reactive variable is filled once you stop typing for 500ms.

```scala mdoc:js
import colibri.Subject
import org.scalajs.dom.ext.KeyCode

val component = {
  val text = Subject.behavior("")
  val debounced = Subject.behavior("")
  div(
      input(
        tpe := "text",
        onInput.value --> text,
        onKeyDown(text).debounceMillis(500) --> debounced,
      ),
      div("text: ", text),
      div("debounced: ", debounced),
   )
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```

### Global events

There are helpers for getting streams of global `document` or `window` events as a handy `colibri.Observable`:

```scala mdoc:js
import colibri.Observable
import org.scalajs.dom.document

val component = {
  val onBlur = events.window.onBlur.map(_ => "blur")
  val onFocus = events.window.onFocus.map(_ => "focus")
  div(
    div("document.onKeyDown: ", events.document.onKeyDown.map(_.key)),
    div("window focus: ", Observable.merge(onBlur, onFocus))
  )
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```


## Referential transparency

When looking at our counter component, you might have noticed that the reactive variable `number` is instantiated immediately. It belongs to our component value `counter`. Let's see what happens when we're using this component twice:

```scala mdoc:js
import colibri.Subject
val counter = {
  val number = Subject.behavior(0)
  div(
      button(number, onClick(number.map(_ + 1)) --> number),
   )
}

val component = {
  div(
    "Counters sharing state:",
    counter,
    counter
  )
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```

As you can see, the state is shared between all usages of the component. Therefore, the component counter is not referentially transparent. We can change this, by wrapping the component in `IO` (or here: `SyncIO` for immediate rendering). With this change, the reactive variable `number` is instantiated separately for every usage at rendering time:


```scala mdoc:js
import colibri.Subject
import cats.effect.SyncIO

val counter = SyncIO { // <-- added IO
  val number = Subject.behavior(0)
  div(
      button(number, onClick(number.map(_ + 1)) --> number),
   )
}

val component = {
  div(
    "Referentially transparent counters:",
    counter,
    counter
  )
}

OutWatch.renderInto[IO](preview, component).unsafeRunSync()
```

Why should we care? Because referentially transparent components can easily be refactored without affecting the meaning of the program. Therefore it is easier to reason about them. Read more about the concept in Wikipedia: [Referential transparency](https://en.wikipedia.org/wiki/Referential_transparency)


### Rendering Async Effects

You can render any `cats.effect.Effect` type like `cats.effect.IO` or `monix.eval.Task`. The effect will be run async whenever an element is rendered with this modifier. So you can do:
```scala mdoc:js:compile-only
import cats.effect.IO

div(
  IO {
    // doSomething
    "result"
  }
)
```

If you have an effect that is synchronous in nature, you should consider using a sync effect type instead for performance reasons. A sync effect can be rendered in one go, whereas an async effect might need to patch the dom after it is finished.

### Rendering Sync Effects

You can render synchronous effects like `cats.effect.SyncIO` or `monix.eval.Coeval` as well (we currently use our own typeclass `colibri.effect.RunSyncEffect` and will port to `cats.effect.SyncEffect` as soon as it is available). The effect will be run sync whenever an element is rendered with this modifier. Example:
```scala mdoc:js:compile-only
import cats.effect.SyncIO

div(
  SyncIO {
    // doSomething
    "result"
  }
)
```

Alternatively you can do the following to achieve the same effect:
```scala mdoc:js:compile-only
div(
  VDomModifier.delay {
    // doSomething
    "result"
  }
)
```



## Advanced

### Accessing the DOM Element

Sometimes you need to access the underlying DOM element of a component. But a VNode in OutWatch is just a description of a dom element and we can create multiple different elements from one VNode. Therefore, there is no static element attached to a component. Though, you can get access to the dom element via hooks (callbacks):
```scala mdoc:js:compile-only
div(
  onDomMount.foreach { element => // the element into which this node is mounted
      ???
  },
  onDomUnmount.foreach { element => // the element from which this node is unmounted
      ???
  }
)
```

Outwatch has a higher-level API to work with these kinds of callbacks, called `managedElement`, which can be used like this:
```scala mdoc:js:compile-only
div(
  managedElement { element => // the element into which this node is mounted
    ??? // do something with the dom element
    cancelable(() => ???) // cleanup when the dom element is unmounted
  }
)
```

You can also get the current element when handling dom events, for example onClick:
```scala mdoc:js:compile-only
div(
  onClick.asElement.foreach { element =>
    ???
  } // this is the same as onClick.map(_.currentTarget)
)
```

If the emitter does not emit events or elements, but you still want to access the current element, you can combine it with another emitter. For example:
```scala mdoc:js:compile-only
import colibri.Observable

val someObservable:Observable[Int] = ???
div(
  emitter(someObservable).useLatestEmitter(onDomMount).foreach { element =>
    ???
    ()
  }
)
```

If you need an HTML or SVG Element instead of just an Element, you can do:
```scala mdoc:js:compile-only
import org.scalajs.dom._

onDomMount.asHtml.foreach{ (elem: html.Element) => ??? }
onDomMount.asSvg.foreach{ (elem: svg.Element) => ??? }
onClick.asHtml.foreach{ (elem: html.Element) => ??? }
onClick.asSvg.foreach{ (elem: svg.Element) => ??? }
managedElement.asHtml { (elem: html.Element) => ???; cancelable(() => ???) }
managedElement.asSvg { (elem: svg.Element) => ???; cancelable(() => ???) }
```

### Custom EmitterBuilders

You can `combine`, `map`, `collect`, `filter` and `transform` `EmitterBuilder`:

```scala mdoc:js:compile-only
button(EmitterBuilder.combine(onMouseUp.use(false), onMouseDown.use(true)).foreach { isDown => println("Button is down? " + isDown) })
// this is the same as: button(EmitterBuilder.combine(onMouseUp.map(_ => false), onMouseDown.map(_ => true)).foreach { isDown => println("Button is down? " + isDown })
```

Furthermore, you can create EmitterBuilders from streams with `emitter(stream)` (`EmitterBuilder.ofSource`) or create custom EmitterBuilders with `EmitterBuilder.ofModifier`, `EmitterBuilder.ofNode` or `EmitterBuilder.apply`.


## Debugging

Source Code: [OutwatchTracing.scala](@REPOURL@/outwatch/src/main/scala/outwatch/helpers/OutwatchTracing.scala)

### Tracing snabbdom patches

Show which patches snabbdom emits:
```scala mdoc:js:compile-only
import scala.scalajs.js.JSON
import org.scalajs.dom.console

helpers.OutwatchTracing.patch.zipWithIndex.foreach { case (proxy, index) =>
  console.log(s"Snabbdom patch ($index)!", JSON.parse(JSON.stringify(proxy)), proxy)
}
```

### Tracing exceptions in your components

Dynamic components with `Observables` can have errors. This is if `onError` is called on the underlying `Observer`. You can trace them in OutWatch with:
```scala mdoc:js:compile-only
import org.scalajs.dom.console

helpers.OutwatchTracing.error.foreach { case throwable =>
  console.log(s"Exception while patching an Outwatch compontent: ${throwable.getMessage}")
}
```
