import java.util.function.Predicate

import com.gargoylesoftware.htmlunit.html.{DomElement, HtmlPage, HtmlSubmitInput, HtmlTextInput}
import org.openqa.selenium.{By, WebDriver, WebElement}
import org.openqa.selenium.firefox.FirefoxDriver

abstract class Element(element: String)

case class ElementId(element: String) extends Element(element)

case class ElementName(element: String) extends Element(element)

case class ElementTag(element: String, css: String, value: String, elementType: String) extends Element(element)

abstract class ElementAction(element: Element) {
  var next: ElementAction = null
  var previous: ElementAction = null

  def and(nextAction: ElementAction): ElementAction = {
    nextAction.previous = this
    next = nextAction
    nextAction
  }

  def firstAction = if (previous == null) this else previous
}

case class WriteIn(element: Element, text: String) extends ElementAction(element)

case class Click(element: Element) extends ElementAction(element)

object Scrawler {
  def navigate(elementAction: => ElementAction, driver: WebDriver): WebDriver = {
    elementAction match {
      case action: WriteIn => action.element match {
        case ElementId(e) => driver.findElement(By.id(e)).sendKeys(action.text)
        case ElementName(e) => driver.findElement(By.name(e)).sendKeys(action.text)
        case ElementTag(e, css, value, elementType) =>
          driver
            .findElements(By.tagName(e))
            .stream()
            .filter(filterByType(elementType))
            .filter(filterByClass(css))
            .filter(filterByValue(value))
            .forEach(e => e.sendKeys(action.text))
      }

      case action: Click => action.element match {
        case ElementId(e) => driver.findElement(By.id(e)).click()
        case ElementName(e) => driver.findElement(By.name(e)).click()
        case ElementTag(e, css, value, elementType) =>
          driver
            .findElements(By.tagName(e))
            .stream()
            .filter(filterByType(elementType))
            .filter(filterByClass(css))
            .filter(filterByValue(value))
            .forEach(e => e.click())
      }
    }

    elementAction match {
      case element if element.next != null => navigate(element.next, driver)
      case _ => driver
    }
  }

  private def filterByValue(value: String): Predicate[WebElement] = {
    el: WebElement => {
      if (value != null) {
        el match {
          case e: HtmlSubmitInput => value.equals(e.getDefaultValue)
          case _ => true
        }
      } else {
        true
      }
    }
  }

  private def filterByClass(css: String): Predicate[WebElement] = {
    el: WebElement => if (css != null) css.equals(el.getAttribute("class")) else true
  }

  private def filterByType(elementType: String): Predicate[WebElement] = {
    el: WebElement => if (elementType != null) elementType.equals(el.getAttribute("type")) else true
  }

  def go(url: String)(elementAction: => ElementAction)(implicit driver: WebDriver): WebDriver = {
    driver.get(url)
    navigate(elementAction.firstAction, driver)
  }

  implicit class DomElementImplicits(element: DomElement) {
    def clickIn: Unit = element.click()
  }

  def id(element: String) = ElementId(element)

  def name(element: String) = ElementName(element)

  def tag(element: String,
          css: String = null,
          value: String = null,
          elementType: String = null) = ElementTag(element, css, value, elementType)

  def write(element: Element)(text: => String): WriteIn = WriteIn(element, text)

  def click(element: Element): Click = Click(element)
}

object Main {
  System.setProperty("webdriver.gecko.driver", "/home/peo_gfsilva/Downloads/geckodriver");

  implicit val driver: WebDriver = new FirefoxDriver

  import Scrawler._

  def main(args: Array[String]): Unit = {
    val result =
      go("http://stackoverflow.com/search") {
        write(tag(element = "input", css = "textbox", elementType = "text")) {
          "spring mvc"
        } and click(tag(element = "input", value = "search", elementType = "submit"))
      }

    println(result.getPageSource)
  }
}
