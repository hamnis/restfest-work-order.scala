package workorder

import java.net.URI
import util.Try
import org.json4s._
import org.json4s.JsonDSL._

case class WorkOrder(_type: URI, input: JValue, start: Option[URI], status: Option[URI], complete: Option[URI], fail: Option[URI]) {
  def as[A](implicit c: InputConverter[A]): A = c.convert(input)
  def asOpt[A](implicit c: InputConverter[A]): Option[A] = Try { as[A](c) }.toOption
  
  def asJson: JValue = {
    val href: (URI) => JString = (href) => href.toString()
    JObject(
      "type" -> _type.toString, 
      "input" -> input, 
      "start" -> start.map(href), 
      "status" -> status.map(href), 
      "complete" -> complete.map(href), 
      "fail" -> fail.map(href)
    )
  }
}

object WorkOrder extends ((URI, JValue, Option[URI], Option[URI], Option[URI], Option[URI]) => WorkOrder) {
  def parse[I](in: JsonInput)(implicit m: JsonMethods[I]) = Parser.parse(in)
}

trait InputConverter[A] {
  def convert(json: JValue): A
}

object WorkOrderConverter {
  implicit val _ident = new InputConverter[JValue] {
    def convert(json: JValue) = json    
  }

  implicit val _asMap = new InputConverter[Map[String, Any]] {
    def convert(json: JValue) = json match {
      case j@JObject(_) => j.values
      case _ => Map.empty
    }
  }
  
}

object Parser {
  def parse[I](rd: JsonInput)(implicit m: JsonMethods[I]): Try[WorkOrder] = Try {
    val result = m.parse(rd)
    val t = toURI(result \ "type")
    val input = result \ "input"
    val start = toURI(result \ "start")
    val status = toURI(result \ "status")
    val complete = toURI(result \ "complete")
    val fail = toURI(result \ "fail")
    WorkOrder(t.getOrElse(sys.error("Missing 'type' property")), input, start, status, complete, fail)
  }

  private def opt(jvalue: JValue) = jvalue match {
    case JNothing | JNull => None
    case a => Some(a)
  }

  private def toURI(jvalue: JValue) = opt(jvalue).map(_.values.toString).map(URI.create)
}
