package ice

import java.io.*
import java.net.{HttpURLConnection, URL}
import java.nio.charset.StandardCharsets
import javax.net.ssl.HttpsURLConnection
import scala.util.Using

object HttpClient {

  var inSecure: Boolean = false

  def escapeUnicode(s: String): String =
    s.map(c => if c <= 0x7f then c else String.format("\\u%04x", c.toInt)).mkString

  def readText(is: InputStream): String = {
    val result = new ByteArrayOutputStream()

    Using.resource(is) { is =>
      val buffer: Array[Byte] = new Array[Byte](1024)
      var length: Int = 0

      while ( {
        length = is.read(buffer)
        length != -1
      }) {
        result.write(buffer, 0, length)
      }
    }

    result.toString("UTF-8")
  }

  def processHttpRequest(url: String, method: String, properties: Map[String, String], body: Object): Either[String, String] = {
    val conn: HttpURLConnection = processHttpRequestSend(url, method, properties, body)

    if (conn.getResponseCode == HttpURLConnection.HTTP_OK) {
      Right(readText(conn.getInputStream))
    } else {
      Left(readText(conn.getErrorStream))
    }
  }

  def saveFile(is: InputStream, fileName: String): File = {
    Using.resource(new BufferedInputStream(is)) { is =>
      TempFileUtil.copyToTempFile(is, fileName)
    }
  }

  def processHttpDownload(url: String, method: String, properties: Map[String, String], body: Object, fileName: String): Either[String, File] = {
    val conn: HttpURLConnection = processHttpRequestSend(url, method, properties, body)

    if (conn.getResponseCode == HttpURLConnection.HTTP_OK) {
      Right(saveFile(conn.getInputStream, fileName))
    } else {
      Left(readText(conn.getErrorStream))
    }
  }

  def processHttpUpload(url: String, method: String, properties: Map[String, String], is: InputStream, limit: Long): Either[String, String] = {
    val conn = processHttpConnect(url, method, properties, is != null)

    val bufferSize = 16 * 1024 * 1024
    val buffer = new Array[Byte](bufferSize)

    Using.resource(new BufferedOutputStream(conn.getOutputStream)) { os =>
      var offset = 0
      while (offset < limit) {
        val len = is.read(buffer, 0, bufferSize min (limit - offset).toInt)
        os.write(buffer, 0, len)
        offset += bufferSize
      }
    }

    if (conn.getResponseCode == HttpURLConnection.HTTP_OK) {
      Right(readText(conn.getInputStream))
    } else {
      Left(readText(conn.getErrorStream))
    }
  }

  def processHttpRequestSend(url: String, method: String, properties: Map[String, String], body: Object): HttpURLConnection = {
    val conn = processHttpConnect(url, method, properties, body != null)

    if (body != null) {
      Using.resource(conn.getOutputStream) { os =>
        os.write(body.toString.getBytes(StandardCharsets.UTF_8))
      }
    }

    conn
  }

  def processHttpConnect(url: String, method: String, properties: Map[String, String], doOutput: Boolean = true, doInput: Boolean = true): HttpURLConnection = {
    val conn = new URL(url).openConnection.asInstanceOf[HttpsURLConnection]
    if (inSecure) {
      conn.setSSLSocketFactory(InSecureSSLContext.instance.getSocketFactory)
    }
    conn.setRequestMethod(method)
    if (properties != null) {
      properties.foreach { (k, v) =>
        conn.setRequestProperty(k, v)
      }
    }
    conn.setDoOutput(doOutput)
    conn.setDoInput(doInput)
    conn.connect()
    conn
  }
}
