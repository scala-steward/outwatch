package outwatch

import cats.effect.IO
import monix.execution.Ack.Continue
import monix.reactive.subjects.PublishSubject
import monix.reactive.Observable
import org.scalajs.dom.{document, html}
import outwatch.dom.helpers._
import outwatch.dom._
import outwatch.dom.dsl._
import outwatch.Deprecated.IgnoreWarnings.initEvent
import snabbdom.{DataObject, hFunction}
import org.scalajs.dom.window.localStorage

import scala.collection.immutable.Seq
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.collection.mutable

class OutWatchDomSpec extends JSDomSpec {

  "Properties" should "be separated correctly" in {
    val properties = Seq(
      Attribute("hidden", "true"),
      InsertHook(PublishSubject()),
      UpdateHook(PublishSubject()),
      InsertHook(PublishSubject()),
      DestroyHook(PublishSubject()),
      PrePatchHook(PublishSubject()),
      PostPatchHook(PublishSubject())
    )

    val SeparatedProperties(att, hooks, keys) = properties.foldRight(SeparatedProperties())((p, sp) => p :: sp)

    hooks.insertHooks.length shouldBe 2
    hooks.prePatchHooks.length shouldBe 1
    hooks.updateHooks.length shouldBe 1
    hooks.postPatchHooks.length shouldBe 1
    hooks.destroyHooks.length shouldBe 1
    att.attrs.length shouldBe 1
    keys.length shouldBe 0
  }

  "VDomModifiers" should "be separated correctly" in {
    val modifiers = Seq(
      Attribute("class", "red"),
      EmptyModifier,
      Emitter("click", _ => Continue),
      new StringVNode("Test"),
      div().unsafeRunSync(),
      CompositeModifier(
        Seq(
          div(),
          attributes.`class` := "blue",
          attributes.onClick(1) --> sideEffect{},
          attributes.hidden <-- Observable(false)
        ).map(_.unsafeRunSync())
      ),
      ModifierStreamReceiver(Observable())
    )

    val SeparatedModifiers(properties, emitters, children) =
      SeparatedModifiers.from(modifiers)

    emitters.emitters.length shouldBe 2
    properties.attributes.attrs.length shouldBe 2
    children.nodes.length shouldBe 5
    children.hasStream shouldBe true
    children.hasVTree shouldBe true
  }

  it should "be separated correctly with children" in {
    val modifiers: Seq[Modifier] = Seq(
      Attribute("class","red"),
      EmptyModifier,
      Emitter("click", _ => Continue),
      Emitter("input",  _ => Continue),
      ModifierStreamReceiver(Observable()),
      ModifierStreamReceiver(Observable()),
      Emitter("keyup",  _ => Continue),
      StringVNode("text"),
      div().unsafeRunSync()
    )

    val SeparatedModifiers(properties, emitters, children) =
      SeparatedModifiers.from(modifiers)

    emitters.emitters.length shouldBe 3
    properties.attributes.attrs.length shouldBe 1
    children.nodes.length shouldBe 4
    children.hasStream shouldBe true
    children.hasVTree shouldBe true
  }

  it should "be separated correctly with string children" in {
    val modifiers: Seq[Modifier] = Seq(
      Attribute("class","red"),
      EmptyModifier,
      Emitter("click", _ => Continue),
      Emitter("input",  _ => Continue),
      Emitter("keyup",  _ => Continue),
      ModifierStreamReceiver(Observable()),
      ModifierStreamReceiver(Observable()),
      StringVNode("text"),
      StringVNode("text2")
    )

    val SeparatedModifiers(properties, emitters, children) =
      SeparatedModifiers.from(modifiers)

    emitters.emitters.length shouldBe 3
    properties.attributes.attrs.length shouldBe 1
    children.nodes.length shouldBe 4
    children.hasStream shouldBe true
    children.hasVTree shouldBe false
  }

  it should "be separated correctly with children and properties" in {
    val modifiers = Seq(
      Attribute("class","red"),
      EmptyModifier,
      Emitter("click", _ => Continue),
      Emitter("input", _ => Continue),
      UpdateHook(PublishSubject()),
      ModifierStreamReceiver(Observable()),
      ModifierStreamReceiver(Observable()),
      ModifierStreamReceiver(Observable()),
      Emitter("keyup", _ => Continue),
      InsertHook(PublishSubject()),
      PrePatchHook(PublishSubject()),
      PostPatchHook(PublishSubject()),
      StringVNode("text")
    )

    val SeparatedModifiers(properties, emitters, children) =
      SeparatedModifiers.from(modifiers)

    emitters.emitters.map(_.eventType) shouldBe List("click", "input", "keyup")
    emitters.emitters.length shouldBe 3
    properties.hooks.insertHooks.length shouldBe 1
    properties.hooks.prePatchHooks.length shouldBe 1
    properties.hooks.updateHooks.length shouldBe 1
    properties.hooks.postPatchHooks.length shouldBe 1
    properties.hooks.destroyHooks.length shouldBe 0
    properties.attributes.attrs.length shouldBe 1
    properties.keys.length shouldBe 0
    children.nodes.length shouldBe 4
    children.hasStream shouldBe true
    children.hasVTree shouldBe false
  }

  val fixture = new {
    val proxy = hFunction("div", DataObject(js.Dictionary("class" -> "red", "id" -> "msg"), js.Dictionary()), js.Array(
      hFunction("span", DataObject(js.Dictionary(), js.Dictionary()), "Hello")
    ))
  }

  it should "be run once" in {
    val list = new collection.mutable.ArrayBuffer[String]

    val vtree = div(
      IO {
        list += "child1"
        ModifierStreamReceiver(Observable(div()))
      },
      IO {
        list += "child2"
        ModifierStreamReceiver(Observable())
      },
      IO {
        list += "children1"
        ModifierStreamReceiver(Observable())
      },
      IO {
        list += "children2"
        ModifierStreamReceiver(Observable())
      },
      div(
        IO {
          list += "attr1"
          Attribute("attr1", "peter")
        },
        Seq(
          IO {
            list += "attr2"
            Attribute("attr2", "hans")
          }
        )
      )
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    list.isEmpty shouldBe true

    OutWatch.renderInto(node, vtree).unsafeRunSync()

    list should contain theSameElementsAs List(
      "child1", "child2", "children1", "children2", "attr1", "attr2"
    )
  }

  it should "provide unique key for child nodes if stream is present" in {
    val mods = Seq(
      ModifierStreamReceiver(Observable()),
      div(id := "1").unsafeRunSync(),
      div(id := "2").unsafeRunSync()
      // div().unsafeRunSync(), div().unsafeRunSync() //TODO: this should also work, but key is derived from hashCode of VTree (which in this case is equal)
    )

    val modifiers =  SeparatedModifiers.from(mods)
    val children = modifiers.children

    children.nodes.length shouldBe 3
    children.hasStream shouldBe true
    children.hasVTree shouldBe true

    val proxy = modifiers.toSnabbdom("div")
    proxy.key.isDefined shouldBe true

    proxy.children.get.length shouldBe 2

    val key1 = proxy.children.get(0).key
    val key2 = proxy.children.get(1).key

    key1.isDefined shouldBe true
    key2.isDefined shouldBe true
    key1.get should not be key2.get
  }

  it should "keep existing key for child nodes" in {
    val mods = Seq(
      Key(1234),
      ModifierStreamReceiver(Observable()),
      div()(IO.pure(Key(5678))).unsafeRunSync()
    )

    val modifiers =  SeparatedModifiers.from(mods)
    val children = modifiers.children

    children.nodes.length shouldBe 2
    children.hasStream shouldBe true
    children.hasVTree shouldBe true

    val proxy = modifiers.toSnabbdom("div")
    proxy.key.toOption  shouldBe Some(1234)

    proxy.children.get(0).key.toOption shouldBe Some(5678)
  }

  "VTrees" should "be constructed correctly" in {

    val attributes = List(Attribute("class", "red"), Attribute("id", "msg"))
    val message = "Hello"
    val child = span(message)
    val vtree = div(IO.pure(attributes.head), IO.pure(attributes(1)), child)

    val proxy = fixture.proxy

    JSON.stringify(vtree.map(_.toSnabbdom).unsafeRunSync()) shouldBe JSON.stringify(proxy)

  }

  it should "be correctly created with the HyperscriptHelper" in {
    val attributes = List(Attribute("class", "red"), Attribute("id", "msg"))
    val message = "Hello"
    val child = span(message)
    val vtree = div(IO.pure(attributes.head), IO.pure(attributes(1)), child)

    JSON.stringify(vtree.map(_.toSnabbdom).unsafeRunSync()) shouldBe JSON.stringify(fixture.proxy)
  }


  it should "run its modifiers once!" in {
    val stringHandler = Handler.create[String]().unsafeRunSync()
    var ioCounter = 0
    var handlerCounter = 0
    stringHandler { _ =>
      handlerCounter += 1
    }

    val vtree = div(
      div(
        IO {
          ioCounter += 1
          Attribute("hans", "")
        }
      ),
      stringHandler
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    ioCounter shouldBe 0
    handlerCounter shouldBe 0
    OutWatch.renderInto(node, vtree).unsafeRunSync()
    ioCounter shouldBe 1
    handlerCounter shouldBe 0
    stringHandler.onNext("pups")
    ioCounter shouldBe 1
    handlerCounter shouldBe 1
  }

  it should "run its modifiers once in CompositeModifier!" in {
    val stringHandler = Handler.create[String]().unsafeRunSync()
    var ioCounter = 0
    var handlerCounter = 0
    stringHandler { _ =>
      handlerCounter += 1
    }

    val vtree = div(
      div(Seq(
        IO {
          ioCounter += 1
          Attribute("hans", "")
        }
      )),
      stringHandler
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    ioCounter shouldBe 0
    handlerCounter shouldBe 0
    OutWatch.renderInto(node, vtree).unsafeRunSync()
    ioCounter shouldBe 1
    handlerCounter shouldBe 0
    stringHandler.onNext("pups")
    ioCounter shouldBe 1
    handlerCounter shouldBe 1
  }

  it should "be correctly patched into the DOM" in {
    val id = "msg"
    val cls = "red"
    val attributes = List(Attribute("class", cls), Attribute("id", id))
    val message = "Hello"
    val child = span(message)
    val vtree = div(IO.pure(attributes.head), IO.pure(attributes(1)), child)


    val node = document.createElement("div")
    document.body.appendChild(node)

    OutWatch.renderInto(node, vtree).unsafeRunSync()

    val patchedNode = document.getElementById(id)

    patchedNode.childElementCount shouldBe 1
    patchedNode.classList.contains(cls) shouldBe true
    patchedNode.children(0).innerHTML shouldBe message

  }

  it should "be replaced if they contain changeables" in {

    def page(num: Int): VNode = {
      val pageNum = Handler.create[Int](num).unsafeRunSync()

      div( id := "page",
        num match {
          case 1 =>
            div(pageNum)
          case 2 =>
            div(pageNum)
        }
      )
    }

    val pageHandler = PublishSubject[Int]

    val vtree = div(
      div(pageHandler.map(page))
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    OutWatch.renderInto(node, vtree).unsafeRunSync()

    pageHandler.onNext(1)

    val domNode = document.getElementById("page")

    domNode.textContent shouldBe "1"

    pageHandler.onNext(2)

    domNode.textContent shouldBe "2"

  }

  "The HTML DSL" should "construct VTrees properly" in {
    import outwatch.dom._

    val vtree = div(cls := "red", id := "msg",
      span("Hello")
    )

    JSON.stringify(vtree.map(_.toSnabbdom).unsafeRunSync()) shouldBe JSON.stringify(fixture.proxy)
  }

  it should "construct VTrees with optional children properly" in {
    import outwatch.dom._

    val vtree = div(cls := "red", id := "msg",
      Option(span("Hello")),
      Option.empty[VDomModifier]
    )

    JSON.stringify(vtree.map(_.toSnabbdom).unsafeRunSync()) shouldBe JSON.stringify(fixture.proxy)

  }

  it should "construct VTrees with boolean attributes" in {
    import outwatch.dom._

    def boolBuilder(name: String) = new BasicAttrBuilder[Boolean](name, identity)
    def stringBuilder(name: String) = new BasicAttrBuilder[Boolean](name, _.toString)
    val vtree = div(
      boolBuilder("a"),
      boolBuilder("b") := true,
      boolBuilder("c") := false,
      stringBuilder("d"),
      stringBuilder("e") := true,
      stringBuilder("f") := false
    )

    val attrs = js.Dictionary[dom.Attr.Value]("a" -> true, "b" -> true, "c" -> false, "d" -> "true", "e" -> "true", "f" -> "false")
    val expected = hFunction("div", DataObject(attrs, js.Dictionary()))

    JSON.stringify(vtree.map(_.toSnabbdom).unsafeRunSync()) shouldBe JSON.stringify(expected)

  }

  it should "patch into the DOM properly" in {
    import outwatch.dom._

    val message = "Test"
    val vtree = div(cls := "blue", id := "test",
      span(message),
      ul(id := "list",
        li("1"),
        li("2"),
        li("3")
      )
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    OutWatch.renderInto(node, vtree).unsafeRunSync()

    val patchedNode = document.getElementById("test")

    patchedNode.childElementCount shouldBe 2
    patchedNode.classList.contains("blue") shouldBe true
    patchedNode.children(0).innerHTML shouldBe message

    document.getElementById("list").childElementCount shouldBe 3

  }

  it should "change the value of a textfield" in {
    val messages = PublishSubject[String]
    val vtree = div(
      input(attributes.value <-- messages, id := "input")
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    OutWatch.renderInto(node, vtree).unsafeRunSync()

    val field = document.getElementById("input").asInstanceOf[html.Input]

    field.value shouldBe ""

    val message = "Hello"
    messages.onNext(message)

    field.value shouldBe message

    val message2 = "World"
    messages.onNext(message2)

    field.value shouldBe message2
  }

  it should "render child nodes in correct order" in {
    val messagesA = PublishSubject[String]
    val messagesB = PublishSubject[String]
    val vNode = div(
      span("A"),
      messagesA.map(span(_)),
      span("B"),
      messagesB.map(span(_))
    )

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    messagesA.onNext("1")
    messagesB.onNext("2")

    node.innerHTML shouldBe "<div><span>A</span><span>1</span><span>B</span><span>2</span></div>"
  }

  it should "render child string-nodes in correct order" in {
    val messagesA = PublishSubject[String]
    val messagesB = PublishSubject[String]
    val vNode = div(
      "A",
      messagesA,
      "B",
      messagesB
    )

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    node.innerHTML shouldBe "<div>AB</div>"

    messagesA.onNext("1")
    node.innerHTML shouldBe "<div>A1B</div>"

    messagesB.onNext("2")
    node.innerHTML shouldBe "<div>A1B2</div>"
  }

  it should "render child string-nodes in correct order, mixed with children" in {
    val messagesA = PublishSubject[String]
    val messagesB = PublishSubject[String]
    val messagesC = PublishSubject[Seq[VNode]]
    val vNode = div(
      "A",
      messagesA,
      messagesC,
      "B",
      messagesB
    )

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    node.innerHTML shouldBe "<div>AB</div>"

    messagesA.onNext("1")
    node.innerHTML shouldBe "<div>A1B</div>"

    messagesB.onNext("2")
    node.innerHTML shouldBe "<div>A1B2</div>"

    messagesC.onNext(Seq(div("5"), div("7")))
    node.innerHTML shouldBe "<div>A1<div>5</div><div>7</div>B2</div>"
  }

  it should "update merged nodes children correctly" in {
    val messages = PublishSubject[Seq[VNode]]
    val otherMessages = PublishSubject[Seq[VNode]]
    val vNode = div(messages)(otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    otherMessages.onNext(Seq(div("otherMessage")))
    node.children(0).innerHTML shouldBe "<div>otherMessage</div>"

    messages.onNext(Seq(div("message")))
    node.children(0).innerHTML shouldBe "<div>message</div><div>otherMessage</div>"

    otherMessages.onNext(Seq(div("genus")))
    node.children(0).innerHTML shouldBe "<div>message</div><div>genus</div>"
  }

  it should "update merged nodes separate children correctly" in {
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(messages)(otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    node.children(0).innerHTML shouldBe ""

    otherMessages.onNext("otherMessage")
    node.children(0).innerHTML shouldBe "otherMessage"

    messages.onNext("message")
    node.children(0).innerHTML shouldBe "messageotherMessage"

    otherMessages.onNext("genus")
    node.children(0).innerHTML shouldBe "messagegenus"
  }

  it should "partially render component even if parts not present" in {
    val messagesColor = PublishSubject[String]
    val messagesBgColor = PublishSubject[String]
    val childString = PublishSubject[String]

    val vNode = div( id := "inner",
      color <-- messagesColor,
      backgroundColor <-- messagesBgColor,
      childString
    )

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()
    val inner = document.getElementById("inner").asInstanceOf[html.Div]

    inner.innerHTML shouldBe ""
    inner.style.color shouldBe ""
    inner.style.backgroundColor shouldBe ""

    childString.onNext("fish")
    inner.innerHTML shouldBe "fish"
    inner.style.color shouldBe ""
    inner.style.backgroundColor shouldBe ""

    messagesColor.onNext("red")
    inner.innerHTML shouldBe "fish"
    inner.style.color shouldBe "red"
    inner.style.backgroundColor shouldBe ""

    messagesBgColor.onNext("blue")
    inner.innerHTML shouldBe "fish"
    inner.style.color shouldBe "red"
    inner.style.backgroundColor shouldBe "blue"
  }

  it should "partially render component even if parts not present2" in {
    val messagesColor = PublishSubject[String]
    val childString = PublishSubject[String]

    val vNode = div( id := "inner",
      color <-- messagesColor,
      childString
    )

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()
    val inner = document.getElementById("inner").asInstanceOf[html.Div]

    inner.innerHTML shouldBe ""
    inner.style.color shouldBe ""

    childString.onNext("fish")
    inner.innerHTML shouldBe "fish"
    inner.style.color shouldBe ""

    messagesColor.onNext("red")
    inner.innerHTML shouldBe "fish"
    inner.style.color shouldBe "red"
  }

  it should "update reused vnodes correctly" in {
    val messages = PublishSubject[String]
    val vNode = div(data.ralf := true, messages)
    val container = div(vNode, vNode)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, container).unsafeRunSync()

    messages.onNext("message")
    node.children(0).children(0).innerHTML shouldBe "message"
    node.children(0).children(1).innerHTML shouldBe "message"

    messages.onNext("bumo")
    node.children(0).children(0).innerHTML shouldBe "bumo"
    node.children(0).children(1).innerHTML shouldBe "bumo"
  }

  it should "update merged nodes correctly (render reuse)" in {
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNodeTemplate = div(messages)
    val vNode = vNodeTemplate(otherMessages)

    val node1 = document.createElement("div")
    document.body.appendChild(node1)
    OutWatch.renderInto(node1, vNodeTemplate).unsafeRunSync()

    val node2 = document.createElement("div")
    document.body.appendChild(node2)
    OutWatch.renderInto(node2, vNode).unsafeRunSync()

    messages.onNext("gurkon")
    otherMessages.onNext("otherMessage")
    node1.children(0).innerHTML shouldBe "gurkon"
    node2.children(0).innerHTML shouldBe "gurkonotherMessage"

    messages.onNext("message")
    node1.children(0).innerHTML shouldBe "message"
    node2.children(0).innerHTML shouldBe "messageotherMessage"

    otherMessages.onNext("genus")
    node1.children(0).innerHTML shouldBe "message"
    node2.children(0).innerHTML shouldBe "messagegenus"
  }

  it should "update merged node attributes correctly" in {
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(data.noise <-- messages)(data.noise <-- otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    otherMessages.onNext("otherMessage")
    node.children(0).getAttribute("data-noise") shouldBe "otherMessage"

    messages.onNext("message") // should be ignored
    node.children(0).getAttribute("data-noise") shouldBe "otherMessage"

    otherMessages.onNext("genus")
    node.children(0).getAttribute("data-noise") shouldBe "genus"
  }

  it should "update merged node styles written with style() correctly" in {
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(style("color") <-- messages)(style("color") <-- otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    otherMessages.onNext("red")
    node.children(0).asInstanceOf[html.Element].style.color shouldBe "red"

    messages.onNext("blue") // should be ignored
    node.children(0).asInstanceOf[html.Element].style.color shouldBe "red"

    otherMessages.onNext("green")
    node.children(0).asInstanceOf[html.Element].style.color shouldBe "green"
  }

  it should "update merged node styles correctly" in {
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(color <-- messages)(color <-- otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    otherMessages.onNext("red")
    node.children(0).asInstanceOf[html.Element].style.color shouldBe "red"

    messages.onNext("blue") // should be ignored
    node.children(0).asInstanceOf[html.Element].style.color shouldBe "red"

    otherMessages.onNext("green")
    node.children(0).asInstanceOf[html.Element].style.color shouldBe "green"
  }


  it should "render composite VNodes properly" in {
    val items = Seq("one", "two", "three")
    val vNode = div(items.map(item => span(item)))

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    node.innerHTML shouldBe "<div><span>one</span><span>two</span><span>three</span></div>"

  }

  it should "render nodes with only attribute receivers properly" in {
    val classes = PublishSubject[String]
    val vNode = button( className <-- classes, "Submit")

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    classes.onNext("active")

    node.innerHTML shouldBe """<button class="active">Submit</button>"""
  }

  it should "work with custom tags" in {

    val vNode = div(tag("main")())

    val node = document.createElement("div")
    document.body.appendChild(node)

    OutWatch.renderInto(node, vNode).unsafeRunSync()

    node.innerHTML shouldBe "<div><main></main></div>"
  }

  it should "work with un-assigned booleans attributes and props" in {

    val vNode = option(selected, disabled)

    val node = document.createElement("option").asInstanceOf[html.Option]
    document.body.appendChild(node)

    node.selected shouldBe false
    node.disabled shouldBe false

    OutWatch.renderReplace(node, vNode).unsafeRunSync()

    node.selected shouldBe true
    node.disabled shouldBe true
  }

  it should "correctly work with AsVDomModifier conversions" in {

    val node = document.createElement("div")

    OutWatch.renderReplace(node, div("one")).unsafeRunSync()
    node.innerHTML shouldBe "one"

    OutWatch.renderReplace(node, div(Some("one"))).unsafeRunSync()
    node.innerHTML shouldBe "one"

    val node2 = document.createElement("div")
    OutWatch.renderReplace(node2, div(None:Option[Int])).unsafeRunSync()
    node2.innerHTML shouldBe ""

    OutWatch.renderReplace(node, div(1)).unsafeRunSync()
    node.innerHTML shouldBe "1"

    OutWatch.renderReplace(node, div(1.0)).unsafeRunSync()
    node.innerHTML shouldBe "1"

    OutWatch.renderReplace(node, div(Seq("one", "two"))).unsafeRunSync()
    node.innerHTML shouldBe "onetwo"

    OutWatch.renderReplace(node, div(Seq(1, 2))).unsafeRunSync()
    node.innerHTML shouldBe "12"

    OutWatch.renderReplace(node, div(Seq(1.0, 2.0))).unsafeRunSync()
    node.innerHTML shouldBe "12"
  }

  "Children stream" should "work for string sequences" in {
    val myStrings: Observable[Seq[String]] = Observable(Seq("a", "b"))
    val node = div(id := "strings",
      myStrings
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "ab"
  }

  "Children stream" should "work for double/boolean/long/int" in {
    val node = div(id := "strings",
      1.1, true, 133L, 7
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "1.1true1337"
  }

  "Child stream" should "work for string options" in {
    val myOption: Handler[Option[String]] = Handler.create(Option("a")).unsafeRunSync()
    val node = div(id := "strings",
      myOption
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "a"

    myOption.onNext(None)
    element.innerHTML shouldBe ""
  }

  it should "work for vnode options" in {
    val myOption: Handler[Option[VNode]] = Handler.create(Option(div("a"))).unsafeRunSync()
    val node = div(id := "strings",
      myOption
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div>a</div>"

    myOption.onNext(None)
    element.innerHTML shouldBe ""
  }

  "Modifier stream" should "work for modifier" in {
    val myHandler = Handler.create[VDomModifier](Seq(cls := "hans", b("stark"))).unsafeRunSync()
    val node = div(id := "strings",
      div(IO.pure(ModifierStreamReceiver(myHandler)))
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe """<div class="hans"><b>stark</b></div>"""

    myHandler.onNext(Option(id := "fair"))
    element.innerHTML shouldBe """<div id="fair"></div>"""
  }

  it should "work for multiple mods" in {
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val node = div(id := "strings",
      div(IO.pure(ModifierStreamReceiver(myHandler)), "bla")
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div>bla</div>"

    myHandler.onNext(cls := "hans")
    element.innerHTML shouldBe """<div class="hans">bla</div>"""

    val innerHandler = Handler.create[VDomModifier]().unsafeRunSync()
    myHandler.onNext(div(
      IO.pure(ModifierStreamReceiver(innerHandler)),
      cls := "no?",
      "yes?"
    ))

    element.innerHTML shouldBe """<div><div class="no?">yes?</div>bla</div>"""

    innerHandler.onNext(Seq(span("question:"), id := "heidi"))
    element.innerHTML shouldBe """<div><div class="no?" id="heidi"><span>question:</span>yes?</div>bla</div>"""

    myHandler.onNext(div(
      IO.pure(ModifierStreamReceiver(innerHandler)),
      cls := "no?",
      "yes?",
      b("go!")
    ))

    element.innerHTML shouldBe """<div><div class="no?">yes?<b>go!</b></div>bla</div>"""

    innerHandler.onNext(Seq(span("question and answer:"), id := "heidi"))
    element.innerHTML shouldBe """<div><div class="no?" id="heidi"><span>question and answer:</span>yes?<b>go!</b></div>bla</div>"""

    myHandler.onNext(Seq(span("nope")))
    element.innerHTML shouldBe """<div><span>nope</span>bla</div>"""

    innerHandler.onNext(b("me?"))
    element.innerHTML shouldBe """<div><span>nope</span>bla</div>"""
  }

  it should "work for nested modifier stream receiver" in {
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val node = div(id := "strings",
      div(IO.pure(ModifierStreamReceiver(myHandler)))
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div></div>"

    val innerHandler = Handler.create[VDomModifier]().unsafeRunSync()
    myHandler.onNext(IO.pure(ModifierStreamReceiver(innerHandler)))
    element.innerHTML shouldBe """<div></div>"""

    innerHandler.onNext(VDomModifier(cls := "hans", "1"))
    element.innerHTML shouldBe """<div class="hans">1</div>"""

    val innerHandler2 = Handler.create[VDomModifier]().unsafeRunSync()
    myHandler.onNext(IO.pure(ModifierStreamReceiver(innerHandler2)))
    element.innerHTML shouldBe """<div></div>"""

    myHandler.onNext(IO.pure(CompositeModifier(ModifierStreamReceiver(innerHandler2) :: Nil)))
    element.innerHTML shouldBe """<div></div>"""

    myHandler.unsafeOnNext(IO.pure(CompositeModifier(StringVNode("pete") :: ModifierStreamReceiver(innerHandler2) :: Nil)))
    element.innerHTML shouldBe """<div>pete</div>"""

    innerHandler2.onNext(VDomModifier(id := "dieter", "r"))
    element.innerHTML shouldBe """<div id="dieter">peter</div>"""

    innerHandler.onNext(b("me?"))
    element.innerHTML shouldBe """<div id="dieter">peter</div>"""

    myHandler.onNext(span("the end"))
    element.innerHTML shouldBe """<div><span>the end</span></div>"""
  }

  it should "work for nested modifier stream receiver and default value" in {
    var numPatches = 0
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val node = div(id := "strings",
      div(
        onPrePatch --> sideEffect { (e1, e2) =>
          numPatches += 1
        },
        IO.pure(ModifierStreamReceiver(myHandler, VDomModifier("initial").unsafeRunSync))
      )
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div>initial</div>"
    numPatches shouldBe 0

    val innerHandler = Handler.create[VDomModifier]().unsafeRunSync()
    myHandler.unsafeOnNext(IO.pure(ModifierStreamReceiver(innerHandler, Attribute("initial", "2"))))
    element.innerHTML shouldBe """<div initial="2"></div>"""
    numPatches shouldBe 1

    innerHandler.unsafeOnNext(IO.pure(Attribute("attr", "3")))
    element.innerHTML shouldBe """<div attr="3"></div>"""
    numPatches shouldBe 2

    val innerHandler2 = Handler.create[VDomModifier]().unsafeRunSync()
    myHandler.unsafeOnNext(IO.pure(ModifierStreamReceiver(innerHandler2, VDomModifier("initial3").unsafeRunSync)))
    element.innerHTML shouldBe """<div>initial3</div>"""
    numPatches shouldBe 3

    myHandler.unsafeOnNext(IO.pure(CompositeModifier(ModifierStreamReceiver(innerHandler2, VDomModifier("initial4").unsafeRunSync) :: Nil)))
    element.innerHTML shouldBe """<div>initial4</div>"""
    numPatches shouldBe 4

    myHandler.unsafeOnNext(IO.pure(CompositeModifier(StringVNode("pete") :: ModifierStreamReceiver(innerHandler2) :: Nil)))
    element.innerHTML shouldBe """<div>pete</div>"""
    numPatches shouldBe 5

    innerHandler2.unsafeOnNext(VDomModifier(id := "dieter", "r"))
    element.innerHTML shouldBe """<div id="dieter">peter</div>"""
    numPatches shouldBe 6

    innerHandler.unsafeOnNext("me?")
    element.innerHTML shouldBe """<div id="dieter">peter</div>"""
    numPatches shouldBe 6

    myHandler.unsafeOnNext(span("the end"))
    element.innerHTML shouldBe """<div><span>the end</span></div>"""
    numPatches shouldBe 7
  }

  it should "work for nested modifier stream receiver and empty default and start with" in {
    var numPatches = 0
    val myHandler = Handler.create[VDomModifier]("initial").unsafeRunSync()
    val node = div(id := "strings",
      div(
        onPrePatch --> sideEffect { (e1, e2) =>
          numPatches += 1
        },
        IO.pure(ModifierStreamReceiver(myHandler))
      )
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div>initial</div>"
    numPatches shouldBe 1

    val innerHandler = Handler.create[VDomModifier]().unsafeRunSync()
    myHandler.unsafeOnNext(IO.pure(ModifierStreamReceiver(innerHandler.startWith(IO.pure(Attribute("initial", "2")) :: Nil))))
    element.innerHTML shouldBe """<div initial="2"></div>"""
    numPatches shouldBe 3

    innerHandler.unsafeOnNext(IO.pure(Attribute("attr", "3")))
    element.innerHTML shouldBe """<div attr="3"></div>"""
    numPatches shouldBe 4

    val innerHandler2 = Handler.create[VDomModifier]().unsafeRunSync()
    myHandler.unsafeOnNext(IO.pure(ModifierStreamReceiver(innerHandler2.startWith("initial3" :: Nil))))
    element.innerHTML shouldBe """<div>initial3</div>"""
    numPatches shouldBe 6

    myHandler.unsafeOnNext(IO.pure(CompositeModifier(ModifierStreamReceiver(innerHandler2.startWith("initial4" :: Nil)) :: Nil)))
    element.innerHTML shouldBe """<div>initial4</div>"""
    numPatches shouldBe 8

    myHandler.unsafeOnNext(IO.pure(CompositeModifier(StringVNode("pete") :: ModifierStreamReceiver(innerHandler2) :: Nil)))
    element.innerHTML shouldBe """<div>pete</div>"""
    numPatches shouldBe 9

    innerHandler2.unsafeOnNext(VDomModifier(id := "dieter", "r"))
    element.innerHTML shouldBe """<div id="dieter">peter</div>"""
    numPatches shouldBe 10

    innerHandler.unsafeOnNext("me?")
    element.innerHTML shouldBe """<div id="dieter">peter</div>"""
    numPatches shouldBe 10

    myHandler.unsafeOnNext(span("the end"))
    element.innerHTML shouldBe """<div><span>the end</span></div>"""
    numPatches shouldBe 11
  }

  it should "work for modifier stream receiver and streaming default value (subscriptions are canceled properly)" in {
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val innerHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val outerTriggers = new scala.collection.mutable.ArrayBuffer[VDomModifier]
    val innerTriggers = new scala.collection.mutable.ArrayBuffer[VDomModifier]
    val node = div(id := "strings",
      div(
        IO.pure(ModifierStreamReceiver(myHandler.map { x => outerTriggers += x; x }, ModifierStreamReceiver(innerHandler.map { x => innerTriggers += x; x }, StringVNode("initial"))))
      )
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div>initial</div>"
    outerTriggers.size shouldBe 0
    innerTriggers.size shouldBe 0

    innerHandler.unsafeOnNext(VDomModifier("hi!"))
    element.innerHTML shouldBe """<div>hi!</div>"""
    outerTriggers.size shouldBe 0
    innerTriggers.size shouldBe 1

    myHandler.unsafeOnNext(VDomModifier("test"))
    element.innerHTML shouldBe """<div>test</div>"""
    outerTriggers.size shouldBe 1
    innerTriggers.size shouldBe 1

    myHandler.unsafeOnNext(IO.pure(ModifierStreamReceiver(Observable.empty, Attribute("initial", "2"))))
    element.innerHTML shouldBe """<div initial="2"></div>"""
    outerTriggers.size shouldBe 2
    innerTriggers.size shouldBe 1

    innerHandler.unsafeOnNext(VDomModifier("me?"))
    element.innerHTML shouldBe """<div initial="2"></div>"""
    outerTriggers.size shouldBe 2
    innerTriggers.size shouldBe 1

    myHandler.unsafeOnNext(IO.pure(ModifierStreamReceiver(innerHandler.map { x => innerTriggers += x; x }, EmptyModifier)))
    element.innerHTML shouldBe """<div></div>"""
    outerTriggers.size shouldBe 3
    innerTriggers.size shouldBe 1

    innerHandler.unsafeOnNext(IO.pure(Attribute("attr", "3")))
    element.innerHTML shouldBe """<div attr="3"></div>"""
    outerTriggers.size shouldBe 3
    innerTriggers.size shouldBe 2

    val innerTriggers2 = new scala.collection.mutable.ArrayBuffer[VDomModifier]
    val innerHandler2 = Handler.create[VDomModifier]().unsafeRunSync()
    val innerTriggers3 = new scala.collection.mutable.ArrayBuffer[VDomModifier]
    val innerHandler3 = Handler.create[VDomModifier]().unsafeRunSync()
    innerHandler.unsafeOnNext(IO.pure(ModifierStreamReceiver(innerHandler2.map { x => innerTriggers2 += x; x }, ModifierStreamReceiver(innerHandler3.map { x => innerTriggers3 += x; x }))))
    element.innerHTML shouldBe """<div></div>"""
    outerTriggers.size shouldBe 3
    innerTriggers.size shouldBe 3
    innerTriggers2.size shouldBe 0
    innerTriggers3.size shouldBe 0

    innerHandler2.unsafeOnNext(VDomModifier("2"))
    element.innerHTML shouldBe """<div>2</div>"""
    outerTriggers.size shouldBe 3
    innerTriggers.size shouldBe 3
    innerTriggers2.size shouldBe 1
    innerTriggers3.size shouldBe 0

    innerHandler.unsafeOnNext(IO.pure(EmptyModifier))
    element.innerHTML shouldBe """<div></div>"""
    outerTriggers.size shouldBe 3
    innerTriggers.size shouldBe 4
    innerTriggers2.size shouldBe 1
    innerTriggers3.size shouldBe 0

    innerHandler2.unsafeOnNext(VDomModifier("me?"))
    element.innerHTML shouldBe """<div></div>"""
    outerTriggers.size shouldBe 3
    innerTriggers.size shouldBe 4
    innerTriggers2.size shouldBe 1
    innerTriggers3.size shouldBe 0

    innerHandler3.unsafeOnNext(VDomModifier("me?"))
    element.innerHTML shouldBe """<div></div>"""
    outerTriggers.size shouldBe 3
    innerTriggers.size shouldBe 4
    innerTriggers2.size shouldBe 1
    innerTriggers3.size shouldBe 0

    myHandler.unsafeOnNext(VDomModifier("go away"))
    element.innerHTML shouldBe """<div>go away</div>"""
    outerTriggers.size shouldBe 4
    innerTriggers.size shouldBe 4
    innerTriggers2.size shouldBe 1
    innerTriggers3.size shouldBe 0

    innerHandler.unsafeOnNext(VDomModifier("me?"))
    element.innerHTML shouldBe """<div>go away</div>"""
    outerTriggers.size shouldBe 4
    innerTriggers.size shouldBe 4
    innerTriggers2.size shouldBe 1
    innerTriggers3.size shouldBe 0
  }

  it should "work for nested observables with seq modifiers " in {
    val innerHandler = Handler.create("b").unsafeRunSync()
    val outerHandler = Handler.create(Seq[VDomModifier]("a", data.test := "v", innerHandler)).unsafeRunSync
    val node = div(
      id := "strings",
      outerHandler
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.outerHTML shouldBe """<div id="strings" data-test="v">ab</div>"""

    innerHandler.onNext("c")
    element.outerHTML shouldBe """<div id="strings" data-test="v">ac</div>"""

    outerHandler.onNext(Seq[VDomModifier]("meh"))
    element.outerHTML shouldBe """<div id="strings">meh</div>"""
  }

  it should "work for nested observables with seq modifiers and attribute stream" in {
    val innerHandler = Handler.create[String]().unsafeRunSync()
    val outerHandler = Handler.create(Seq[VDomModifier]("a", data.test := "v", href <-- innerHandler)).unsafeRunSync
    val node = div(
      id := "strings",
      outerHandler
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.outerHTML shouldBe """<div id="strings" data-test="v">a</div>"""

    innerHandler.onNext("c")
    element.outerHTML shouldBe """<div id="strings" data-test="v" href="c">a</div>"""

    innerHandler.onNext("d")
    element.outerHTML shouldBe """<div id="strings" data-test="v" href="d">a</div>"""

    outerHandler.onNext(Seq[VDomModifier]("meh"))
    element.outerHTML shouldBe """<div id="strings">meh</div>"""
  }

  it should "work for double nested modifier stream receiver" in {
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val node = div(id := "strings",
      div(IO.pure(ModifierStreamReceiver(myHandler)))
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div></div>"

    myHandler.onNext(IO.pure(ModifierStreamReceiver(Observable[VDomModifier](IO.pure(ModifierStreamReceiver(Observable[VDomModifier](cls := "hans")))))))
    element.innerHTML shouldBe """<div class="hans"></div>"""
  }

  it should "work for triple nested modifier stream receiver" in {
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val node = div(id := "strings",
      div(IO.pure(ModifierStreamReceiver(myHandler)))
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div></div>"

    myHandler.onNext(IO.pure(ModifierStreamReceiver(Observable[VDomModifier](IO.pure(ModifierStreamReceiver(Observable[VDomModifier](IO.pure(ModifierStreamReceiver(Observable(cls := "hans"))))))))))
    element.innerHTML shouldBe """<div class="hans"></div>"""
  }

  it should "work for multiple nested modifier stream receiver" in {
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val node = div(id := "strings",
      div(IO.pure(ModifierStreamReceiver(myHandler)))
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div></div>"

    myHandler.onNext(IO.pure(ModifierStreamReceiver(Observable[VDomModifier](VDomModifier(IO.pure(ModifierStreamReceiver(Observable[VDomModifier]("a"))), IO.pure(ModifierStreamReceiver(Observable(span("b")))))))))
    element.innerHTML shouldBe """<div>a<span>b</span></div>"""
  }

  it should "work for nested attribute stream receiver" in {
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val node = div(id := "strings",
      div(IO.pure(ModifierStreamReceiver(myHandler)))
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div></div>"

    myHandler.onNext(cls <-- Observable("hans"))
    element.innerHTML shouldBe """<div class="hans"></div>"""
  }

  it should "work for nested emitter" in {
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val node = div(id := "strings",
      div(id := "click", IO.pure(ModifierStreamReceiver(myHandler)))
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe """<div id="click"></div>"""

    var clickCounter = 0
    myHandler.onNext(onClick --> sideEffect(_ => clickCounter += 1))
    element.innerHTML shouldBe """<div id="click"></div>"""

    clickCounter shouldBe 0
    val event = document.createEvent("Events")
    initEvent(event)("click", canBubbleArg = true, cancelableArg = false)
    document.getElementById("click").dispatchEvent(event)
    clickCounter shouldBe 1

    document.getElementById("click").dispatchEvent(event)
    clickCounter shouldBe 2

    myHandler.unsafeOnNext(VDomModifier.empty)
    element.innerHTML shouldBe """<div id="click"></div>"""

    document.getElementById("click").dispatchEvent(event)
    clickCounter shouldBe 2
  }

  it should "work for streaming accum attributes" in {
    val myClasses = Handler.create[String]("second").unsafeRunSync()
    val myClasses2 = Handler.create[String]().unsafeRunSync()
    val node = div(
      id := "strings",
      div(
        cls := "first",
        myClasses.map { cls := _ },
        Seq[VDomModifier](
          cls <-- myClasses2
        )
      )
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")

    element.innerHTML shouldBe """<div class="first second"></div>"""

    myClasses2.onNext("third")
    element.innerHTML shouldBe """<div class="first second third"></div>"""

    myClasses2.onNext("more")
    element.innerHTML shouldBe """<div class="first second more"></div>"""

    myClasses.onNext("yeah")
    element.innerHTML shouldBe """<div class="first yeah more"></div>"""
  }

  "LocalStorage" should "provide a handler" in {

    val key = "banana"
    val triggeredHandlerEvents = mutable.ArrayBuffer.empty[Option[String]]

    assert(localStorage.getItem(key) == null)

    val storageHandler = util.LocalStorage.handler(key).unsafeRunSync()
    storageHandler.foreach{e => triggeredHandlerEvents += e}
    assert(localStorage.getItem(key) == null)
    assert(triggeredHandlerEvents.toList == List(None))

    storageHandler.onNext(Some("joe"))
    assert(localStorage.getItem(key) == "joe")
    assert(triggeredHandlerEvents.toList == List(None, Some("joe")))

    var initialValue:Option[String] = null
    util.LocalStorage.handler(key).unsafeRunSync().foreach {initialValue = _}
    assert(initialValue == Some("joe"))

    storageHandler.onNext(None)
    assert(localStorage.getItem(key) == null)
    assert(triggeredHandlerEvents.toList == List(None, Some("joe"), None))

    // localStorage.setItem(key, "split") from another window
    dispatchStorageEvent(key, newValue = "split", null)
    assert(localStorage.getItem(key) == "split")
    assert(triggeredHandlerEvents.toList == List(None, Some("joe"), None, Some("split")))

    // localStorage.removeItem(key) from another window
    dispatchStorageEvent(key, null, "split")
    assert(localStorage.getItem(key) == null)
    assert(triggeredHandlerEvents.toList == List(None, Some("joe"), None, Some("split"), None))

    // only trigger handler if value changed
    storageHandler.onNext(None)
    assert(localStorage.getItem(key) == null)
    assert(triggeredHandlerEvents.toList == List(None, Some("joe"), None, Some("split"), None))

    storageHandler.onNext(Some("rhabarbar"))
    assert(localStorage.getItem(key) == "rhabarbar")
    assert(triggeredHandlerEvents.toList == List(None, Some("joe"), None, Some("split"), None, Some("rhabarbar")))

    // localStorage.clear() from another window
    dispatchStorageEvent(null, null, null)
    assert(localStorage.getItem(key) == null)
    assert(triggeredHandlerEvents.toList == List(None, Some("joe"), None, Some("split"), None, Some("rhabarbar"), None))
  }
}
