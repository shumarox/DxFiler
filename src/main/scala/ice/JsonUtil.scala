package ice

import javax.script.ScriptEngineManager
import scala.collection.{mutable, Map as SMap}

object JsonUtil {
  private val engine = new ScriptEngineManager().getEngineByName("nashorn")

  private def eval(text: String): Object = engine.eval(text)

  private val scriptObjectClass = Class.forName("jdk.nashorn.api.scripting.ScriptObjectMirror")

  private def isScriptObject(obj: Object): Boolean = scriptObjectClass.isInstance(obj)

  private val keySetMethod = scriptObjectClass.getMethod("keySet")

  private def getJsObjectKeys(obj: Object): Array[AnyRef] = {
    if (obj == null) return Array[Object]()

    keySetMethod.invoke(obj) match {
      case null => Array[Object]()
      case s: java.util.Set[_] => s.toArray
    }
  }

  private def isArray(obj: Object): Boolean = {
    getJsObjectKeys(obj).zipWithIndex.forall((a, b) => a.toString == b.toString)
  }

  private val getMethod = scriptObjectClass.getMethod("get", classOf[Object])

  private def getValue(obj: Object, key: Object): Object = getMethod.invoke(obj, key)

  private def makeJsString(text: String): String = {
    "'" + text
      .replaceAll("\r?\n", " \\\\\n\\\\n")
      .replaceAll("'", "\\\\'")
      .replaceAll("\"", "\\\\\"") + "'"
  }

  def jsonStringToMap(text: String): SMap[String, Object] = {
    if (text == null) {
      null
    } else if (text.isEmpty) {
      Map()
    } else {
      jsObjectToMap(eval(s"JSON.parse(${makeJsString(text)})"))
    }
  }

  private def jsObjectToMap(obj: Object): SMap[String, Object] = {
    new mutable.LinkedHashMap[String, Object].++=(
      getJsObjectKeys(obj).map { key =>
        getValue(obj, key) match {
          case null =>
            (key.toString, null)
          case value if isScriptObject(value) =>
            (key.toString, if (isArray(value)) jsObjectToList(value) else jsObjectToMap(value))
          case value: java.lang.Double =>
            (key.toString, BigDecimal(value).bigDecimal.toPlainString.replaceAll("\\.0$", ""))
          case value =>
            (key.toString, value.toString)
        }
      }
    )
  }

  private def jsObjectToList(obj: Object): List[Object] = {
    getJsObjectKeys(obj).map { key =>
      getValue(obj, key) match {
        case null =>
          null
        case value if isScriptObject(value) =>
          if (isArray(value)) jsObjectToList(value) else jsObjectToMap(value)
        case value: java.lang.Double =>
          BigDecimal(value).bigDecimal.toPlainString.replaceAll("\\.0$", "")
        case value =>
          value.toString
      }
    }.toList
  }

  private def escapeDQ: String => String = s => "\"" + s.replaceAll("\"", "\\\\\"") + "\""

  def mapToJsonString(map: SMap[String, Object]): String = {
    if (map == null) {
      null
    } else if (map.isEmpty) {
      ""
    } else {
      map.filterNot {
        (key, _) => key == null
      }.map {
        case (key, null) => (key, "null")
        case (key, value: SMap[_, _]) => (key, mapToJsonString(value.asInstanceOf[SMap[String, Object]]))
        case (key, value: Seq[_]) => (key, arrayToJsonString(value.asInstanceOf[Seq[Object]]))
        case (key, value: Object) => (key, escapeDQ(value.toString))
      }.map {
        (key, value) => escapeDQ(key) + ":" + value
      }.mkString("{", ",", "}")
    }
  }

  private def arrayToJsonString(array: Seq[_]): String = {
    array.map {
      case null => "null"
      case value: SMap[_, _] => mapToJsonString(value.asInstanceOf[SMap[String, Object]])
      case value: Seq[_] => arrayToJsonString(value.asInstanceOf[Seq[Object]])
      case value => escapeDQ(value.toString)
    }.mkString("[", ",", "]")
  }

  def jsonStringToJsonString(text: String): String = {
    if (text == null) {
      null
    } else if (text.isEmpty) {
      ""
    } else {
      eval(s"JSON.stringify($text)").toString
    }
  }

  def jsonStringToJsonString(text: String, indentString: String): String = {
    if (text == null) {
      null
    } else if (text.isEmpty) {
      ""
    } else {
      eval(s"JSON.stringify($text, null, ${escapeDQ(indentString)})").toString
    }
  }
}