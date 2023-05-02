package ice

import org.json.JSONObject

import scala.jdk.CollectionConverters.*

object JsonUtil {

  def convert(text: String): Map[String, Object] = convert(new JSONObject(text).toMap).asInstanceOf[Map[String, Object]]

  private def convert(m: java.util.Map[_, _]): Map[_, _] =
    m.asScala.map {
      case (k, l: java.util.List[_]) =>
        (k, convert(l))
      case (k, m: java.util.Map[_, _]) =>
        (k, convert(m))
      case (k, o) =>
        (k, o)
    }.toMap

  private def convert(l: java.util.List[_]): List[_] =
    l.asScala.map {
      case v: java.util.List[_] => convert(v)
      case m: java.util.Map[_, _] => convert(m)
      case o => o
    }.toList

}