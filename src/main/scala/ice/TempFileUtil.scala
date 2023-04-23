package ice

import java.io.{File, InputStream}
import java.nio.file.{Files, StandardCopyOption}
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.atomic.AtomicInteger

object TempFileUtil {
  private val tempNo: AtomicInteger = new AtomicInteger

  lazy val tempRootDir: File = new File(System.getProperty("java.io.tmpdir"))

  lazy val tempDir: File = {
    val dir = new File(tempRootDir, "DxFiler" + new SimpleDateFormat("yyyyMMddhhmmss").format(new Date))
    dir.mkdir
    dir.deleteOnExit()
    dir
  }

  def makeWorkDirectory(): File = {
    val workDir = new File(tempDir, tempNo.incrementAndGet().toString)
    workDir.mkdir
    workDir.deleteOnExit()

    workDir
  }

  def copyToTempFile(file: File): File = {
    val workDir = makeWorkDirectory()

    val tempFile = new File(workDir, file.getName)
    Files.copy(file.toPath, tempFile.toPath, StandardCopyOption.REPLACE_EXISTING)
    tempFile.deleteOnExit()

    tempFile
  }

  def copyToTempFile(inputStream: InputStream, filename: String): File = {
    val workDir = makeWorkDirectory()

    val tempFile = new File(workDir, filename)
    Files.copy(inputStream, tempFile.toPath, StandardCopyOption.REPLACE_EXISTING)
    tempFile.deleteOnExit()

    tempFile
  }
}
