package ice

import java.awt.datatransfer.{Clipboard, StringSelection, Transferable}
import java.awt.event.InputEvent
import java.awt.{Desktop, Toolkit}
import java.io.File
import java.net.URL
import java.text.SimpleDateFormat
import java.util.Date
import javax.swing.*
import javax.swing.border.EmptyBorder
import javax.swing.filechooser.FileSystemView
import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer, TableColumnModel}
import scala.collection.mutable
import scala.swing.BorderPanel.Position.{Center, East, South}
import scala.swing.event.{Key, KeyPressed, MouseClicked, TableRowsSelected}
import scala.swing.{Action, *}
import scala.util.boundary
import scala.util.boundary.break

object SharedLinkListView {
  private val TITLE = "Shared link list"

  private val DIRECTORY_ICON = UIManager.getIcon("FileView.directoryIcon")
  private val FILE_ICON = UIManager.getIcon("FileView.fileIcon")

  private val fileSystemView: FileSystemView = FileSystemView.getFileSystemView
  private val desktop: Desktop = Desktop.getDesktop

  private var frame: Frame = _

  private val fileTableModel: FileTableModel = new FileTableModel

  private val table: FileTable = new FileTable

  private var currentDirectoryInfo: String = ""

  table.listenTo(table.mouse.clicks)
  table.listenTo(table.keys)
  table.reactions += {
    case _: TableRowsSelected =>
      val viewRows = table.selection.rows
      val model = table.model.asInstanceOf[FileTableModel]
      val count = viewRows.size
      lbStatus.text = currentDirectoryInfo + "  " + f"$count%,d items selected."
    case ev: MouseClicked if ev.clicks == 2 =>
      getSelectedFiles.foreach { file =>
        executeAndShowError {
          openBrowser(file)
        }
      }
      ev.consume()
    case ev: MouseClicked
      if ev.clicks == 1 && (ev.modifiers & InputEvent.META_DOWN_MASK) != 0 =>
      showTablePopupMenu(table, ev.point.x, ev.point.y)
    case ev: MouseClicked =>
      ev.consume()
    case ev: KeyPressed if ev.key == Key.Enter && ev.modifiers == 0 =>
      var firstDirectory = true

      getSelectedFiles.foreach { file =>
        executeAndShowError {
          openBrowser(file)
        }
      }
      ev.consume()
    case ev: KeyPressed if ev.key == Key.ContextMenu =>
      val r = table.peer.getCellRect(table.selection.rows.anchorIndex, table.selection.columns.anchorIndex, true)

      val rx = r.x + r.width
      val ry = r.y + r.height

      showTablePopupMenu(table, rx, ry)

      ev.consume()
    case ev: KeyPressed if ev.key == Key.F5 =>
      refresh()
      ev.consume()
    case ev: KeyPressed if ev.key == Key.Delete =>
      if (getSelectedFiles.nonEmpty) {
        Dialog.showConfirmation(frame, "共有リンクを削除しますか？", TITLE, Dialog.Options.OkCancel) match {
          case Dialog.Result.Ok =>
            executeAndShowError {
              revokeSharedLink(getSelectedFiles)
            }
          case _ =>
        }
      }
  }

  private def openBrowser(file: DxFile): Unit = desktop.browse(new URL(file.sharedLinkUrl).toURI)

  private val lbStatus: Label = new Label("") {
    border = new EmptyBorder(1, 1, 1, 1)
    horizontalAlignment = Alignment.Leading
  }

  def show(): Unit = boundary {
    if (frame != null) {
      if (frame.visible) {
        frame.visible = true
      }
      frame.peer.requestFocus()
      break()
    }

    frame = new Frame {
      title = TITLE

      override def closeOperation(): Unit = {
        frame.dispose()
        frame = null
      }
    }

    val tableScroll: ScrollPane = new ScrollPane(table) {
      preferredSize = new Dimension(1366, preferredSize.getHeight.toInt)

      listenTo(mouse.clicks, horizontalScrollBar.mouse.clicks, verticalScrollBar.mouse.clicks)
      reactions += {
        case ev@MouseClicked(source, point, Key.Modifier.Meta, 1, _) =>
          ev.consume()
          table.selection.rows.clear()
          showTablePopupMenu(source, point.x, point.y)
      }
    }

    val tablePanel: BorderPanel = new BorderPanel {
      layout(tableScroll) = Center
      layout(new BorderPanel {
        layout(lbStatus) = Center
        layout(WaitCursorWorker.progressBar) = East
      }) = South
    }

    frame.contents = tablePanel

    refresh()

    frame.visible = true
  }

  private class FileTable extends Table {
    override def apply(row: Int, column: Int): Any = model.getValueAt(viewToModelRow(row), viewToModelColumn(column))

    autoResizeMode = Table.AutoResizeMode.Off
    selection.elementMode = Table.ElementMode.Row
    selection.intervalMode = Table.IntervalMode.MultiInterval
    peer.setShowVerticalLines(false)
    peer.setAutoCreateRowSorter(true)

    model = fileTableModel

    rowHeight = fileSystemView.getSystemIcon(new File(".")).getIconHeight + 3

    FileTableModel.setColumnWidth(peer.getColumnModel)

    font = new Font("Monospaced", font.getStyle, font.getSize)

    peer.setDefaultRenderer(classOf[java.lang.Long], new LongValueTableCellRenderer)
  }

  private object FileTableModel {

    private case class ColumnInfo(name: String, clazz: Class[_], minChars: Int = -1, maxChars: Int = -1, preferredChars: Int = -1)

    private val columns: Array[ColumnInfo] = {
      Array(
        ColumnInfo("Icon", classOf[ImageIcon], 4, 4, 4),
        ColumnInfo("Path(lowered)", classOf[String], -1, -1, 60),
        ColumnInfo("Size", classOf[java.lang.Long], -1, -1, 13),
        ColumnInfo("Last Modified", classOf[String], 19, 19, 19),
        ColumnInfo("ID", classOf[String], -1, -1, 26),
        ColumnInfo("Shared link URL", classOf[String], -1, -1, 60),
      )
    }

    def setColumnWidth(tableColumnModel: TableColumnModel): Unit = {
      class MyLabel(text: String) extends Label(text) {
        font = new Font("Monospaced", font.getStyle, font.getSize)
      }

      def calcWidth(chars: Int): Int =
        new MyLabel("W" * chars).preferredSize.getWidth.toInt + 10

      columns.zipWithIndex.foreach {
        case (FileTableModel.ColumnInfo(_, _, minChars, maxChars, preferredChars), i) =>
          val tableColumn = tableColumnModel.getColumn(i)

          if (minChars >= 0) tableColumn.setMinWidth(calcWidth(minChars))
          if (maxChars >= 0) tableColumn.setMaxWidth(calcWidth(maxChars))
          if (preferredChars >= 0) tableColumn.setPreferredWidth(calcWidth(preferredChars))
      }
    }
  }

  private class FileTableModel(var files: Array[DxFile]) extends AbstractTableModel {

    import FileTableModel.*

    def this() = this(new Array[DxFile](0))

    override def getColumnCount: Int = columns.length

    override def getColumnName(column: Int): String = columns(column).name

    override def getColumnClass(column: Int): Class[_] = columns(column).clazz

    private val iconMap: mutable.Map[String, Icon] = mutable.Map[String, Icon]()

    private def getIcon(file: File): Icon = {
      if (file.isDirectory) {
        DIRECTORY_ICON
      } else {
        val name = file.getName
        val dotPos = name.lastIndexOf(".")
        val ext = if (dotPos < 0) "" else name.substring(dotPos + 1)

        if (ext.isEmpty) {
          FILE_ICON
        } else {
          iconMap.getOrElseUpdate(ext, {
            val dummyFile = File.createTempFile("DummyForGetIcon", "." + ext)
            val icon = try fileSystemView.getSystemIcon(dummyFile) finally dummyFile.delete()
            Option(icon).getOrElse(FILE_ICON)
          })
        }
      }
    }

    override def getValueAt(row: Int, column: Int): Object = {
      val file = files(row)

      columns(column).name match {
        case "Icon" => getIcon(file)
        case "Path(lowered)" => file.getPath
        case "Size" => if (file.isDirectory) null else java.lang.Long.valueOf(file.length)
        case "Last Modified" => if (file.lastModified <= 0) null else new SimpleDateFormat("yyyy/MM/dd HH:mm:ss").format(new Date(file.lastModified))
        case "ID" => file.id
        case "Shared link URL" => file.sharedLinkUrl
      }
    }

    override def getRowCount: Int = files.length

    def getFile(row: Int): File = files(row)

    def setFiles(files: Array[DxFile]): Unit = {
      this.files = files
      fireTableDataChanged()
    }
  }

  class LongValueTableCellRenderer extends DefaultTableCellRenderer {
    override def getTableCellRendererComponent(table: JTable, value: Object, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): java.awt.Component = {
      super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
      value match {
        case null =>
        case l: java.lang.Long => setText(f"$l%,3d")
        case _ =>
      }
      setHorizontalAlignment(SwingConstants.RIGHT)
      this
    }
  }

  private def showError(message: String, detail: Object = null): Unit = showMessage(message, detail, Dialog.Message.Error)

  private def showWarning(message: String, detail: Object = null): Unit = showMessage(message, detail, Dialog.Message.Warning)

  private def showMessage(message: String, detail: Object, messageType: Dialog.Message.Value): Unit = {
    val messageForDisplay = if (detail == null || detail.toString.isEmpty) message else message + "\n" + detail
    Dialog.showMessage(frame, messageForDisplay, TITLE, messageType)
  }

  private def executeAndShowError(f: => Unit): Unit = {
    try {
      f
    } catch {
      case t: Throwable =>
        showError("エラーが発生しました。", t)
    }

    frame.repaint()
  }

  private def setTableData(files: Array[DxFile]): Unit = {
    table.deafTo(table.selection)
    fileTableModel.setFiles(files)
    table.listenTo(table.selection)
  }

  private def setCurrentDirectoryInfo(sharedLinkList: Array[DxFile]): Unit = {
    val count = sharedLinkList.length
    currentDirectoryInfo = f"$count%,d items."
    lbStatus.text = currentDirectoryInfo
    frame.repaint()
  }

  private def refresh(): Unit = {
    val sharedLinkList: Array[DxFile] = Dx.listSharedLink("/")
    setTableData(sharedLinkList)
    setCurrentDirectoryInfo(sharedLinkList)
  }

  private def getSelectedFiles: List[DxFile] =
    table.selection.rows.toList.map(table.viewToModelRow).map(table.model.asInstanceOf[FileTableModel].files)

  private def revokeSharedLink(files: Seq[DxFile]): Unit =
    WaitCursorWorker(frame, true) { () =>
      files.foreach { file =>
        Dx.revokeSharedLinkWithErrorMessage(file.sharedLinkUrl)
      }
      refresh()
    }(null).execute()

  private def showTablePopupMenu(invoker: Component, x: Int, y: Int): Unit = {
    new PopupMenu {
      private def createMenuItem(label: String, mnemonic: Key.Value)(f: => Unit): MenuItem = {
        val action = Action(label) {
          executeAndShowError {
            f
          }
        }

        val menuItem = new MenuItem(action)
        menuItem.mnemonic = mnemonic
        menuItem
      }

      val selectedFiles: Seq[DxFile] = getSelectedFiles

      if (selectedFiles.nonEmpty) {
        contents +=
          createMenuItem("Open shared link", Key.O) {
            executeAndShowError {
              selectedFiles.foreach(openBrowser)
            }
          }

        contents +=
          createMenuItem("Delete shared link", Key.D) {
            executeAndShowError {
              revokeSharedLink(selectedFiles)
            }
          }
      }

      if (selectedFiles.nonEmpty) {
        def setToClipboard(text: String): Unit =
          Toolkit.getDefaultToolkit.getSystemClipboard.setContents(new StringSelection(text), (clipboard: Clipboard, contents: Transferable) => {})

        def setPropertyToClipboard(f: DxFile => String): Unit = {
          executeAndShowError {
            val text = selectedFiles.map { case dxFile: DxFile => f(dxFile) }.mkString("\n")
            setToClipboard(text)
          }
        }

        contents += new Menu("Copy property") {
          mnemonic = Key.C
          contents += createMenuItem("Copy file name", Key.N)(setPropertyToClipboard(_.getName))
          contents += createMenuItem("Copy dropbox path(lowered)", Key.P)(setPropertyToClipboard(_.getPath))
          contents += createMenuItem("Copy dropbox ID", Key.I)(setPropertyToClipboard(_.id))
          contents += createMenuItem("Copy shared link URL", Key.U)(setPropertyToClipboard(_.sharedLinkUrl))
          contents += createMenuItem("Copy properties", Key.R) {
            executeAndShowError {
              val text =
                table.selection.rows.toList.map(table.viewToModelRow).map { row =>
                  val sep = "\t"
                  "" +
                    fileTableModel.getValueAt(row, 1) + sep +
                    Option(fileTableModel.getValueAt(row, 2)).getOrElse("") + sep +
                    Option(fileTableModel.getValueAt(row, 3)).getOrElse("") + sep +
                    fileTableModel.getValueAt(row, 4) + sep +
                    fileTableModel.getValueAt(row, 5)
                }.mkString("\n")

              setToClipboard(text)
            }
          }
        }
      }
    }.show(invoker, x, y)
  }
}
