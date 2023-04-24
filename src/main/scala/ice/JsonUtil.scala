package ice

import org.mozilla.javascript.{NativeArray, NativeObject}

import javax.script.ScriptEngineManager
import scala.jdk.CollectionConverters.*

object JsonUtil {
  private val engine = new ScriptEngineManager().getEngineByName("rhino")

  private def makeJsString(text: String): String = {
    "'" + text
      .replaceAll("\r?\n", " \\\\\n\\\\n")
      .replaceAll("'", "\\\\'")
      .replaceAll("\"", "\\\\\"") + "'"
  }

  def jsonStringToMap(text: String): Map[String, Object] =
    if (text == null) {
      null
    } else if (text.isEmpty) {
      Map()
    } else {
      toJava(engine.eval(s"JSON.parse(${makeJsString(text)})")).asInstanceOf[Map[String, Object]]
    }

  private def toJava(o: Object): Object =
    o match {
      case m: NativeObject => toMap(m)
      case l: NativeArray => toList(l)
      case i: java.lang.Integer => i.toString
      case d: java.lang.Double => BigDecimal(d).bigDecimal.toPlainString.replaceAll("\\.0$", "")
      case o => o
    }

  private def toMap(m: NativeObject): Map[String, Object] =
    m.entrySet.asScala.map(e => (e.getKey.toString, toJava(e.getValue))).toMap

  private def toList(l: NativeArray): List[Object] = l.toArray.map(o => toJava(o)).toList
}