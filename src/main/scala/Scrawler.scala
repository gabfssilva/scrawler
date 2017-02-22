import com.gargoylesoftware.htmlunit.{BrowserVersion, WebClient}
import com.gargoylesoftware.htmlunit.html.{DomElement, HtmlPage, HtmlTextInput}

abstract class Element(element: String)

case class ElementId(element: String) extends Element(element)

case class ElementName(element: String) extends Element(element)

case class ElementTag(element: String, css: String, value: String) extends Element(element)

abstract class ElementAction(element: Element) {
  var next: ElementAction = null
  var previous: ElementAction = null

  def and(nextAction: => ElementAction): ElementAction = {
    nextAction.previous = this
    next = nextAction
    nextAction
  }

  def firstAction = if(previous == null) this else previous
}

case class WriteIn(element: Element, text: String) extends ElementAction(element)

case class Click(element: Element) extends ElementAction(element)

object Scrawler {
  def navigate(elementAction: => ElementAction, page: HtmlPage): HtmlPage = {
    elementAction match {
      case action: WriteIn => action.element match {
        case ElementId(e) => page.getElementById(e).asInstanceOf[HtmlTextInput].setText(action.text)
        case ElementName(e) => page.getElementByName[HtmlTextInput](e).setText(action.text)
        case ElementTag(e, css, value) =>
          page
            .getElementsByTagName(e)
            .stream()
            .filter(el => if (css != null) css.equals(el.getAttribute("class")) else true)
            .filter(el => if (value != null) value.equals(el.getNodeValue) else true)
            .forEach(e => e.asInstanceOf[HtmlTextInput].setText(action.text))
      }

      case action: Click => action.element match {
        case ElementId(e) => page.getElementById(e).clickIn
        case ElementName(e) => page.getElementByName[DomElement](e).clickIn
        case ElementTag(e, css, value) =>
          page
            .getElementsByTagName(e)
            .stream()
            .filter(el => if (css != null) css.equals(el.getAttribute("class")) else true)
            .filter(el => if (value != null) value.equals(el.getNodeValue) else true)
            .forEach(e => e.clickIn)
      }
    }

    elementAction match {
      case element if element.next != null => navigate(element.next, page)
      case _ => page
    }
  }

  def go(url: String)(elementAction: => ElementAction)(implicit driver: WebClient): String = {
    val page: HtmlPage = go(url, driver)
    navigate(elementAction.firstAction, page)
    page.asText()
  }

  implicit class DomElementImplicits(element: DomElement) {
    def clickIn: Unit = element.click()
  }

  private def go(url: String, driver: WebClient): HtmlPage = driver.getPage(url)

  def id(element: String) = ElementId(element)

  def name(element: String) = ElementName(element)

  def tag(element: String, css: String = null, value: String = null) = ElementTag(element, css, value)

  def writeIn(element: Element)(text: => String): WriteIn = WriteIn(element, text)

  def click(element: Element): Click = Click(element)
}

object Main {
  implicit val client: WebClient = new WebClient(BrowserVersion.FIREFOX_38)

  import Scrawler._

  def main(args: Array[String]): Unit = {
    val result =
      go("http://stackoverflow.com/search") {
        writeIn(name("q"))("spring mvc") and click(tag(element = "submit", value = "search"))
      }

    println(result)
  }
}
