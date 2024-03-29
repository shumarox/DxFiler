package ice

import net.lingala.zip4j.ZipFile

import java.awt.Desktop
import java.io.*
import java.net.{URI, URL}
import java.nio.charset.StandardCharsets
import java.nio.file.*
import java.nio.file.attribute.*
import java.text.SimpleDateFormat
import java.util
import java.util.{Properties, TimeZone}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.swing.{Dialog, Window}
import scala.util.matching.Regex
import scala.util.{Try, Using}

object Dx {
  import HttpClient.*

  private val PROPERTY_FILE_NAME = "DxFiler.properties"
  private val properties = new Properties

  Using.resource(new FileInputStream(PROPERTY_FILE_NAME)) { is =>
    properties.load(is)
  }

  private val APP_NAME = "DxFiler"
  private val APP_KEY = properties.getProperty("APP_KEY")
  private val APP_SECRET = properties.getProperty("APP_SECRET")

  inSecure = java.lang.Boolean.valueOf(properties.getProperty("IN_SECURE"))

  private var refreshToken: String = properties.getProperty("REFRESH_TOKEN")
  private var accessToken: String = _
  private var expiresIn: Long = 0L
  private var lastTimeAccessToken: Long = 0L

  var parentWindow: Window = _

  private def showError(message: String, detail: Object = null): Unit = {
    val messageForDisplay = if (detail == null || detail.toString.isEmpty) message else message + "\n" + detail
    Dialog.showMessage(parentWindow, messageForDisplay, APP_NAME, Dialog.Message.Error)
  }

  def ensureRefreshToken(): Unit = {
    if (refreshToken == null) {
      Desktop.getDesktop.browse(new URL(s"https://www.dropbox.com/oauth2/authorize?response_type=code&token_access_type=offline&client_id=$APP_KEY").toURI)
      val auth_code =
        Dialog.showInput(parentWindow, message = "ブラウザに表示されたアクセスコードを貼り付けてください。", APP_NAME, initial = "").match {
          case Some(auth_code) =>
            auth_code
          case None =>
            showError("アクセスコードが入力されなかったため、アプリを終了します。")
            System.exit(-1)
            null
        }

      val body = s"code=$auth_code&grant_type=authorization_code&client_id=$APP_KEY&client_secret=$APP_SECRET"
      processHttpRequest("https://api.dropbox.com/oauth2/token", "POST", null, body).match {
        case Right(result) =>
          refreshToken = new Regex("\"refresh_token\" *: *\"([^\"]*)\"").findFirstMatchIn(result).match {
            case Some(m) =>
              m.group(1)
            case None =>
              System.err.println(result)
              showError("認証に失敗したため、アプリを終了します。", result)
              System.exit(-1)
              null
          }
          properties.setProperty("REFRESH_TOKEN", refreshToken)
          Using.resource(new FileOutputStream(PROPERTY_FILE_NAME)) { os =>
            properties.store(os, null)
          }
        case Left(result) =>
          System.err.println(result)
          showError("認証に失敗したため、アプリを終了します。", result)
          System.exit(-1)
      }
    }
  }

  @tailrec
  def ensureAccessToken(): Unit = {
    ensureRefreshToken()

    if (accessToken == null || System.currentTimeMillis - lastTimeAccessToken > expiresIn - 10 * 60 * 1000) {
      accessToken = null
      expiresIn = 0L

      val body = s"grant_type=refresh_token&refresh_token=$refreshToken&client_id=$APP_KEY&client_secret=$APP_SECRET"
      processHttpRequest("https://api.dropbox.com/oauth2/token", "POST", null, body) match {
        case Right(result) =>
          new Regex("\"access_token\" *: *\"([^\"]*)\"").findFirstMatchIn(result).match {
            case Some(m) =>
              lastTimeAccessToken = System.currentTimeMillis()
              accessToken = m.group(1)
              new Regex("\"expires_in\" *: *(\\d+)").findFirstMatchIn(result).match {
                case Some(m) =>
                  expiresIn = m.group(1).toLong * 1000L
                case None =>
                  expiresIn = 0L
              }
            case None =>
              System.err.println(result)
              refreshToken = null
              ensureRefreshToken()
              ensureAccessToken()
          }
        case Left(result) =>
          System.err.println(result)
          refreshToken = null
          ensureRefreshToken()
          ensureAccessToken()
      }
    }
  }

  private var lastListFilePath: String = _
  private var lastListFileTime: Long = 0L
  private var lastListFileResult: Array[DxFile] = _

  def list(path: String): Array[DxFile] = {
    ensureAccessToken()

    val properties = Map("Authorization" -> s"Bearer ${Dx.accessToken}", "Content-Type" -> "application/json")

    def resultToDxFileArray(map: Map[String, Object]): Array[DxFile] = {
      map("entries").asInstanceOf[List[Map[String, Object]]].map { entry =>
        val path: String = entry("path_display").toString
        val isDirectory: Boolean = entry(".tag") == "folder"
        val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"))
        val lastModifiedTime: FileTime = if (isDirectory) null else Try(FileTime.fromMillis(sdf.parse(entry("client_modified").toString.replaceAll("T", " ").dropRight(1)).getTime)).getOrElse(null)
        val size: Long =
          if (isDirectory) 0L else entry("size") match {
            case l: java.lang.Long => l
            case i: Integer => i.longValue
            case _ => 0L
          }
        val id: String = entry("id").toString
        new DxPath(path, new DxFileAttributes(isDirectory, lastModifiedTime, size, id)).toDxFile
      }.toArray
    }

    @tailrec
    def processResultAndNext(result: String): Unit = {
      val map = JsonUtil.convert(result)
      lastListFileResult ++= resultToDxFileArray(map)

      if (map("has_more").toString.toBoolean) {
        val cursor = map("cursor").toString
        val body = s"""{"cursor": "$cursor"}"""

        processHttpRequest("https://api.dropboxapi.com/2/files/list_folder/continue", "POST", properties, body).match {
          case Right(result) =>
            processResultAndNext(result)
          case Left(result) =>
            lastListFileResult = Array()
            System.err.println(result)
            showError("ファイル一覧の取得に失敗しました。", result)
        }
      }
    }

    def listStart(path: String): Unit = {
      val pathForRequest = if (path == "/") "" else path
      val body = s"""{"path": "$pathForRequest"}"""

      processHttpRequest("https://api.dropboxapi.com/2/files/list_folder", "POST", properties, body).match {
        case Right(result) =>
          processResultAndNext(result)
        case Left(result) =>
          lastListFileResult = Array()
          System.err.println(result)
          showError("ファイル一覧の取得に失敗しました。", result)
      }
    }

    if (path != lastListFilePath || System.currentTimeMillis - lastListFileTime >= 100) {
      lastListFileResult = Array()
      listStart(path)
      lastListFilePath = path
      lastListFileTime = System.currentTimeMillis()
    }

    lastListFileResult
  }

  def download(file: DxFile): File = {
    if (file.isDirectory) {
      downloadFolder(file)
    } else {
      downloadFile(file)
    }
  }

  def downloadFile(file: DxFile): File = {
    ensureAccessToken()

    val path = file.toString

    val properties = Map("Authorization" -> s"Bearer ${Dx.accessToken}", "Dropbox-API-Arg" -> s"""{"path": "${escapeUnicode(path)}"}""")
    processHttpDownload("https://content.dropboxapi.com/2/files/download", "POST", properties, null, path.substring(path.lastIndexOf("/") + 1)).match {
      case Right(result) =>
        file.downloaded = result
        result
      case Left(result) =>
        System.err.println(result)
        showError("ダウンロードに失敗しました。", result)
        null
    }
  }

  def downloadFolder(file: DxFile): File = {
    ensureAccessToken()

    val path = file.toString

    val properties = Map("Authorization" -> s"Bearer ${Dx.accessToken}", "Dropbox-API-Arg" -> s"""{"path": "${escapeUnicode(path)}"}""")
    processHttpDownload("https://content.dropboxapi.com/2/files/download_zip", "POST", properties, null, file.getName + ".zip").match {
      case Right(result) =>
        val parent = TempFileUtil.makeWorkDirectory()

        new ZipFile(result.toPath.toString) {
          setCharset(StandardCharsets.UTF_8)
        }.extractAll(parent.toPath.toString)

        result.delete()

        Files.walkFileTree(parent.toPath, new util.HashSet[FileVisitOption], Integer.MAX_VALUE, new FileVisitor[Path]() {
          override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = FileVisitResult.CONTINUE

          override def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
            path.toFile.deleteOnExit()
            FileVisitResult.CONTINUE
          }

          override def visitFileFailed(file: Path, exc: IOException): FileVisitResult = throw exc

          override def postVisitDirectory(path: Path, exc: IOException): FileVisitResult = {
            path.toFile.deleteOnExit()
            FileVisitResult.CONTINUE
          }
        })

        val folder = new File(parent, file.getName)
        file.downloaded = folder
        folder
      case Left(result) =>
        System.err.println(result)
        showError("ダウンロードに失敗しました。", result)
        null
    }
  }

  private case class SourceDestDxFilePair(source: File, dest: DxFile)

  def upload(dest: DxFile, files: Array[File]): Unit = {

    def toDxFile(parentDxFile: DxFile, files: Array[File]): List[SourceDestDxFilePair] = {
      files.toList.flatMap { file =>
        val dxFile = new DxFile(parentDxFile, file)

        if (file.isDirectory) {
          toDxFile(dxFile, file.listFiles)
        } else if (file.length == 0) {
          Nil
        } else {
          List(SourceDestDxFilePair(file, dxFile))
        }
      }
    }

    val pairs: Array[SourceDestDxFilePair] = toDxFile(dest, files).toArray

    if (pairs.isEmpty) {
      showError("空でないファイルがありません。")
      return
    }

    pairs.grouped(1000).foreach(upload)
  }

  private def upload(pairs: Array[SourceDestDxFilePair]): Unit = {
    val properties = Map("Authorization" -> s"Bearer ${Dx.accessToken}", "Content-Type" -> "application/json")
    val bodyForStart = s"""{"num_sessions": ${pairs.length}, "session_type": {".tag": "sequential"}}"""

    val sessionIds =
      processHttpRequest("https://api.dropboxapi.com/2/files/upload_session/start_batch", "POST", properties, bodyForStart).match {
        case Right(result) =>
          val map = JsonUtil.convert(result)
          map("session_ids").asInstanceOf[List[String]].toArray
        case Left(result) =>
          System.err.println(result)
          throw new IllegalStateException(result)
      }

    val chunkSize: Long = 128L * 1024 * 1024

    (pairs zip sessionIds).foreach { case (SourceDestDxFilePair(source, _), sessionId) =>
      var offset: Long = 0L

      Using.resource(new BufferedInputStream(new FileInputStream(source))) { is =>
        while (offset < source.length) {
          val close = offset + chunkSize >= source.length
          val properties = Map("Authorization" -> s"Bearer ${Dx.accessToken}", "Content-Type" -> "application/octet-stream",
            "Dropbox-API-Arg" -> s"""{"cursor": {"session_id": "$sessionId", "offset": $offset}, "close": $close}"""
          )

          val limit: Long = chunkSize min (source.length - offset)

          processHttpUpload("https://content.dropboxapi.com/2/files/upload_session/append_v2", "POST", properties, is, limit).match {
            case Right(_) =>
              System.out.print(".")
            case Left(result) =>
              System.err.println(result)
              throw new IllegalStateException(result)
          }

          offset += chunkSize
        }
      }
    }

    @tailrec
    def waitFinish(asyncJobId: String): String = {
      Thread.sleep(200)

      val body = s"""{"async_job_id": "$asyncJobId"}"""

      processHttpRequest("https://api.dropboxapi.com/2/files/upload_session/finish_batch/check", "POST", properties, body).match {
        case Right(result) =>
          val map = JsonUtil.convert(result)
          if (map(".tag") == "in_progress") {
            waitFinish(asyncJobId)
          } else {
            result
          }
        case Left(result) =>
          System.err.println(result)
          throw new IllegalStateException(result)
      }
    }

    val entries =
      (pairs zip sessionIds).map { case (SourceDestDxFilePair(source, dest), sessionId) =>
        val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"))
        val clientModified = sdf.format(source.lastModified).replaceAll(" ", "T") + "Z"
        s"""{"cursor": {"session_id": "$sessionId", "offset": ${source.length}}, "commit": {"path": "$dest", "mode": {".tag": "overwrite"}, "autorename": false, "client_modified": "$clientModified", "strict_conflict": true}}"""
      }

    val bodyForFinish = s"""{"entries": [${entries.mkString(", ")}]}"""

    processHttpRequest("https://api.dropboxapi.com/2/files/upload_session/finish_batch_v2", "POST", properties, bodyForFinish).match {
      case Right(resultFinish) =>
        var result = resultFinish
        var map: Map[String, Object] = JsonUtil.convert(result)

        if (map.contains("async_job_id")) {
          result = waitFinish(map("async_job_id").toString)
          map = JsonUtil.convert(result)
        } else if (map("entries").asInstanceOf[List[Map[String, Object]]].exists(_(".tag") != "success")) {
          throw new IllegalStateException(result)
        }
      case Left(result) =>
        System.err.println(result)
        throw new IllegalStateException(result)
    }
  }

  def copy(from: String, to: String): Either[String, String] = {
    ensureAccessToken()

    val body = s"""{"from_path": "$from", "to_path": "$to"}"""

    val properties = Map("Authorization" -> s"Bearer ${Dx.accessToken}", "Content-Type" -> "application/json")
    processHttpRequest("https://api.dropboxapi.com/2/files/copy_v2", "POST", properties, body)
  }

  def copyWithErrorMessage(from: String, to: String): Unit = {
    copy(from, to).match {
      case Right(_) =>
      case Left(result) =>
        System.err.println(result)
        showError("コピーに失敗しました。", result)
        Array[DxPath]()
    }
  }

  def delete(path: String): Either[String, String] = {
    ensureAccessToken()

    if (path == "/") throw new IllegalArgumentException("can't remove root path")

    val body = s"""{"path": "$path"}"""

    val properties = Map("Authorization" -> s"Bearer ${Dx.accessToken}", "Content-Type" -> "application/json")
    processHttpRequest("https://api.dropboxapi.com/2/files/delete_v2", "POST", properties, body)
  }

  def deleteWithErrorMessage(path: String): Unit = {
    delete(path).match {
      case Right(_) =>
      case Left(result) =>
        System.err.println(result)
        showError("削除に失敗しました。", result)
        Array[DxPath]()
    }
  }

  def revokeSharedLink(url: String): Either[String, String] = {
    ensureAccessToken()

    val body = s"""{"url": "$url"}"""

    val properties = Map("Authorization" -> s"Bearer ${Dx.accessToken}", "Content-Type" -> "application/json")
    processHttpRequest("https://api.dropboxapi.com/2/sharing/revoke_shared_link", "POST", properties, body)
  }

  def revokeSharedLinkWithErrorMessage(url: String): Unit = {
    revokeSharedLink(url).match {
      case Right(_) =>
      case Left(result) =>
        System.err.println(result)
        showError("共有リンクの削除に失敗しました。", result)
        Array[DxPath]()
    }
  }

  def move(from: String, to: String): Either[String, String] = {
    ensureAccessToken()

    val body = s"""{"from_path": "$from", "to_path": "$to"}"""

    val properties = Map("Authorization" -> s"Bearer ${Dx.accessToken}", "Content-Type" -> "application/json")
    processHttpRequest("https://api.dropboxapi.com/2/files/move_v2", "POST", properties, body)
  }

  def renameWithErrorMessage(from: String, to: String): Unit = {
    move(from, to).match {
      case Right(_) =>
      case Left(result) =>
        System.err.println(result)
        showError("名前の変更に失敗しました。", result)
        Array[DxPath]()
    }
  }

  def createFolder(path: String): Either[String, String] = {
    ensureAccessToken()

    val body = s"""{"path": "$path"}"""

    val properties = Map("Authorization" -> s"Bearer ${Dx.accessToken}", "Content-Type" -> "application/json")
    processHttpRequest("https://api.dropboxapi.com/2/files/create_folder_v2", "POST", properties, body)
  }

  def createFolderWithErrorMessage(path: String): Unit = {
    createFolder(path).match {
      case Right(_) =>
      case Left(result) =>
        System.err.println(result)
        showError("フォルダの作成に失敗しました。", result)
        Array[DxPath]()
    }
  }

  def createSharedLink(path: String): Either[String, String] = {
    ensureAccessToken()

    val body = s"""{"path": "$path"}"""

    val properties = Map("Authorization" -> s"Bearer ${Dx.accessToken}", "Content-Type" -> "application/json")
    processHttpRequest("https://api.dropboxapi.com/2/sharing/create_shared_link_with_settings", "POST", properties, body) match {
      case Right(result) =>
        val map: Map[String, Object] = JsonUtil.convert(result)
        Right(map("url").toString)
      case Left(result) =>
        Left(result)
    }
  }

  def createSharedLinkWithErrorMessage(path: String): String = {
    createSharedLink(path).match {
      case Right(url) => url
      case Left(result) =>
        System.err.println(result)
        showError("共有リンクの作成に失敗しました。", result)
        null
    }
  }

  def listSharedLink(path: String): Array[DxFile] = {
    ensureAccessToken()

    val properties = Map("Authorization" -> s"Bearer ${Dx.accessToken}", "Content-Type" -> "application/json")

    val resultFileList: mutable.ArrayBuffer[DxFile] = new mutable.ArrayBuffer[DxFile]

    def resultToDxFileArray(map: Map[String, Object]): Array[DxFile] = {
      map("links").asInstanceOf[List[Map[String, Object]]].map { entry =>
        val path: String = entry("path_lower").toString
        val isDirectory: Boolean = entry(".tag") == "folder"
        val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"))
        val lastModifiedTime: FileTime = if (isDirectory) null else Try(FileTime.fromMillis(sdf.parse(entry("client_modified").toString.replaceAll("T", " ").dropRight(1)).getTime)).getOrElse(null)
        val size: Long =
          if (isDirectory) 0L else entry("size") match {
            case l: java.lang.Long => l
            case i: Integer => i.longValue
            case _ => 0L
          }
        val id: String = entry("id").toString
        val url: String = entry("url").toString
        new DxPath(path, new DxFileAttributes(isDirectory, lastModifiedTime, size, id, url)).toDxFile
      }.toArray
    }

    @tailrec
    def processResultAndNext(result: String): Unit = {
      val map = JsonUtil.convert(result)
      resultFileList ++= resultToDxFileArray(map)

      if (map("has_more").toString.toBoolean) {
        val cursor = map("cursor").toString
        val body = s"""{"cursor": "$cursor"}"""

        processHttpRequest("https://api.dropboxapi.com/2/sharing/list_shared_links", "POST", properties, body).match {
          case Right(result) =>
            processResultAndNext(result)
          case Left(result) =>
            System.err.println(result)
            showError("共有リンク一覧の取得に失敗しました。", result)
        }
      }
    }

    def listStart(path: String): Unit = {
      val body = if (path == null || path.isEmpty || path == "/") "{}" else s"""{"path": "$path"}"""

      processHttpRequest("https://api.dropboxapi.com/2/sharing/list_shared_links", "POST", properties, body).match {
        case Right(result) =>
          processResultAndNext(result)
        case Left(result) =>
          System.err.println(result)
          showError("共有リンク一覧の取得に失敗しました。", result)
      }
    }

    listStart(path)

    resultFileList.toArray
  }
}

private class DxPath(private val pathString: String, val dxFileAttributes: DxFileAttributes) extends Path with Serializable {

  def this(path: String) = this(path, null: DxFileAttributes)

  def this(parent: String, child: String) = this(parent + (if (parent == "/") "" else "/") + child)

  override def getFileSystem: FileSystem = throw new UnsupportedOperationException

  override def isAbsolute: Boolean = pathString.startsWith("/")

  override def getRoot: Path = DxRootPath

  override lazy val getFileName: Path = {
    val name = if (pathString == "/") "/" else new File(pathString).getName
    new DxPath(name)
  }

  lazy val parent: DxPath = {
    if (pathString == "/") null
    else {
      val s = pathString.take(pathString.lastIndexOf("/"))
      if (s.isEmpty) DxRootPath else new DxPath(s)
    }
  }

  override def getParent: Path = parent

  override def getNameCount: Int = throw new UnsupportedOperationException

  override def getName(index: Int): Path = throw new UnsupportedOperationException

  override def subpath(beginIndex: Int, endIndex: Int): Path = throw new UnsupportedOperationException

  override def startsWith(other: Path): Boolean = throw new UnsupportedOperationException

  override def startsWith(other: String): Boolean = throw new UnsupportedOperationException

  override def endsWith(other: Path): Boolean = throw new UnsupportedOperationException

  override def endsWith(other: String): Boolean = throw new UnsupportedOperationException

  override def normalize(): Path = this

  override def resolve(other: Path): Path = throw new NotImplementedError

  def resolveDx(other: String): DxPath = {
    if (other.isEmpty) {
      this
    } else {
      val newPath = pathString + (if (pathString == "/") "" else "/") + other
      new DxPath(newPath)
    }
  }

  override def resolve(other: String): Path = resolveDx(other)

  override def resolveSibling(other: Path): Path = throw new NotImplementedError

  override def resolveSibling(other: String): Path = throw new NotImplementedError

  override def relativize(other: Path): Path = new DxPath(other.toString.drop(toString.length + 1))

  override def toUri: URI = throw new NotImplementedError

  override def toAbsolutePath: Path = this

  override def toRealPath(options: LinkOption*): Path = this

  lazy val toDxFile: DxFile = new DxFile(this)

  override lazy val toFile: File = toDxFile

  override def register(watcher: WatchService, events: Array[WatchEvent.Kind[_]], modifiers: Array[? <: WatchEvent.Modifier]): WatchKey = throw new NotImplementedError

  override def register(watcher: WatchService, events: Array[? <: WatchEvent.Kind[_]]): WatchKey = throw new NotImplementedError

  override def iterator(): java.util.Iterator[Path] = util.Arrays.asList(Dx.list(pathString).map(_.toPath): _*).iterator

  override def compareTo(other: Path): Int = throw new NotImplementedError

  override def hashCode(): Int = pathString.hashCode

  override def equals(obj: Any): Boolean = {
    obj match {
      case null =>
        false
      case that: DxPath =>
        this.pathString == that.pathString
      case _ =>
        false
    }
  }

  override def toString: String = pathString
}

object DxRootPath extends DxPath("/", RootFileAttributes)

private class DxFileAttributes(override val isDirectory: Boolean, @transient override val lastModifiedTime: FileTime, override val size: Long, val id: String, val sharedLinkUrl: String = null) extends BasicFileAttributes with Serializable {
  @transient
  override val creationTime: FileTime = null

  override def isOther: Boolean = false

  override def isRegularFile: Boolean = !isDirectory

  @transient
  override def lastAccessTime: FileTime = null

  override def isSymbolicLink: Boolean = false

  override def fileKey: Object = null
}

private object RootFileAttributes extends DxFileAttributes(isDirectory = true, lastModifiedTime = null, size = 0, id = null)

class DxFile(val path: DxPath) extends File(path.toAbsolutePath.toString) {
  var downloaded: File = _

  def this(parent: String, child: String) = this(new DxPath(parent, child))


  def this(parent: DxFile, child: String) = this(parent.path.resolveDx(child))

  def this(parent: DxFile, child: File) = this(parent, child.getName)

  def id: String = path.dxFileAttributes.id

  def sharedLinkUrl: String = path.dxFileAttributes.sharedLinkUrl

  private def toStringOrNull(path: Path): String = Option(path).map(_.toString).orNull

  override lazy val getName: String = toStringOrNull(path.getFileName)

  override lazy val getParent: String = toStringOrNull(path.getParent)

  lazy val parentFile: DxFile = Option(path.parent).map(_.toDxFile).orNull

  override lazy val getParentFile: File = parentFile

  override lazy val getPath: String = toStringOrNull(path)

  override def isAbsolute: Boolean = path.isAbsolute

  override lazy val getAbsolutePath: String = toStringOrNull(path)

  override lazy val getAbsoluteFile: File = DxFile(path)

  override lazy val getCanonicalPath: String = toStringOrNull(path)

  override lazy val getCanonicalFile: File = DxFile(path)

  @deprecated override lazy val toURL: URL = path.toUri.toURL

  override lazy val toURI: URI = path.toUri

  override def canRead: Boolean = false

  override def canWrite: Boolean = false

  override def exists: Boolean = throw new UnsupportedOperationException

  override def isDirectory: Boolean = path.dxFileAttributes.isDirectory

  override def isFile: Boolean = path.dxFileAttributes.isRegularFile || path.dxFileAttributes.isSymbolicLink

  override def isHidden: Boolean = throw new UnsupportedOperationException

  override def lastModified: Long = Try(path.dxFileAttributes.lastModifiedTime.toMillis).getOrElse(0L)

  override def length: Long = Try(path.dxFileAttributes.size).getOrElse(0L)

  override def createNewFile: Boolean = throw new UnsupportedOperationException

  override def delete: Boolean = throw new UnsupportedOperationException

  override def deleteOnExit(): Unit = throw new UnsupportedOperationException

  override def list: Array[String] = throw new UnsupportedOperationException

  override def list(filter: FilenameFilter): Array[String] = throw new UnsupportedOperationException

  override def listFiles: Array[File] = Dx.list(path.toString).asInstanceOf[Array[File]]

  override def listFiles(filter: FilenameFilter): Array[File] = listFiles.filter(f => filter.accept(this, f.getName))

  override def listFiles(filter: FileFilter): Array[File] = listFiles.filter(filter.accept)

  override def mkdir: Boolean = throw new UnsupportedOperationException

  override def mkdirs: Boolean = throw new UnsupportedOperationException

  override def renameTo(dest: File): Boolean = throw new UnsupportedOperationException

  override def setLastModified(time: Long): Boolean = throw new UnsupportedOperationException

  override def setReadOnly(): Boolean = throw new UnsupportedOperationException

  override def setWritable(writable: Boolean, ownerOnly: Boolean): Boolean = throw new UnsupportedOperationException

  override def setWritable(writable: Boolean): Boolean = throw new UnsupportedOperationException

  override def setReadable(readable: Boolean, ownerOnly: Boolean): Boolean = throw new UnsupportedOperationException

  override def setReadable(readable: Boolean): Boolean = throw new UnsupportedOperationException

  override def setExecutable(executable: Boolean, ownerOnly: Boolean): Boolean = throw new UnsupportedOperationException

  override def setExecutable(executable: Boolean): Boolean = throw new UnsupportedOperationException

  override def canExecute: Boolean = false

  override def getTotalSpace: Long = throw new UnsupportedOperationException

  override def getFreeSpace: Long = throw new UnsupportedOperationException

  override def getUsableSpace: Long = throw new UnsupportedOperationException

  override def compareTo(pathname: File): Int = path.compareTo(pathname.toPath)

  private def equalsPath(that: Path): Boolean = {
    if (that == null || this.path.getClass != that.getClass) return false
    toStringOrNull(this.path.toAbsolutePath) == toStringOrNull(that.toAbsolutePath)
  }

  override def equals(obj: Any): Boolean =
    obj match {
      case f: File =>
        equalsPath(Try(f.toPath).getOrElse(null))
      case p: Path =>
        equalsPath(p)
      case _ =>
        false
    }

  override lazy val hashCode: Int = path.hashCode()

  override lazy val toString: String = toStringOrNull(path)

  override def toPath: Path = path
}
