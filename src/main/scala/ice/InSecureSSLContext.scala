package ice

import java.security.cert.X509Certificate
import javax.net.ssl.*

object InSecureSSLContext {

  private var _instance: SSLContext = _

  def instance: SSLContext = {
    if (_instance == null) {
      val tm = Array[TrustManager](new X509TrustManager() {
        override def getAcceptedIssuers: Array[X509Certificate] = null

        override def checkClientTrusted(chain: Array[X509Certificate], authType: String): Unit = {}

        override def checkServerTrusted(chain: Array[X509Certificate], authType: String): Unit = {}
      })

      _instance = SSLContext.getInstance("SSL")
      _instance.init(null, tm, null)
    }

    _instance
  }
}
