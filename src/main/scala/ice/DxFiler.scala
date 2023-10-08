package ice

import java.awt.datatransfer.{Clipboard, ClipboardOwner, DataFlavor, StringSelection, Transferable, UnsupportedFlavorException}
import java.awt.dnd.DnDConstants
import java.awt.event.*
import java.awt.{Desktop, Toolkit, Component as AComponent}
import java.io.*
import java.text.SimpleDateFormat
import java.util
import java.util.Date
import javax.swing.*
import javax.swing.border.*
import javax.swing.event.*
import javax.swing.filechooser.FileSystemView
import javax.swing.table.*
import javax.swing.tree.*
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.swing.BorderPanel.Position.*
import scala.swing.Orientation.*
import scala.swing.event.{FocusLost, Key, KeyPressed, MouseClicked, TableRowsSelected}
import scala.swing.{Action, *}
import scala.util.{Failure, Random, Success, Try, boundary}
import scala.util.boundary.break

class DxFileList(list: java.util.List[DxFile]) extends java.util.ArrayList[DxFile](list) with Serializable

object DxFiler {
  private val APP_NAME = "DxFiler"

  private val DIRECTORY_ICON = UIManager.getIcon("FileView.directoryIcon")
  private val FILE_ICON = UIManager.getIcon("FileView.fileIcon")

  private val desktop: Desktop = Desktop.getDesktop
  private val fileSystemView: FileSystemView = FileSystemView.getFileSystemView

  private var frame: Frame = _

  private object DxFileListFlavor extends DataFlavor("application/dx-filer_" + Random.nextInt(Integer.MAX_VALUE).toHexString + "; class=ice.DxFileList")

  private def showError(message: String, detail: Object = null): Unit = showMessage(message, detail, Dialog.Message.Error)
  private def showWarning(message: String, detail: Object = null): Unit = showMessage(message, detail, Dialog.Message.Warning)

  private def showMessage(message: String, detail: Object, messageType: Dialog.Message.Value): Unit = {
    val messageForDisplay = if (detail == null || detail.toString.isEmpty) message else message + "\n" + detail
    Dialog.showMessage(frame, messageForDisplay, APP_NAME, messageType)
  }

  def main(args: Array[String]): Unit = {
    Try(UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName))
    Try(UIManager.put("Button.showMnemonics", true))

    frame = new Frame {
      title = APP_NAME

      override def closeOperation(): Unit = System.exit(0)
    }

    Dx.parentWindow = frame

    val filer = new DxFiler
    frame.contents = filer.mainPanel

    frame.peer.setLocationByPlatform(true)
    frame.pack()
    frame.peer.setSize(new Dimension(frame.size.width, frame.size.height max 600))

    Swing.onEDT {
      frame.visible = true
    }
  }

  class LongValueTableCellRenderer extends DefaultTableCellRenderer {
    override def getTableCellRendererComponent(table: JTable, value: Object, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): AComponent = {
      super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
      value match {
        case null =>
        case l: Long => setText(f"$l%,3d")
        case _ =>
      }
      setHorizontalAlignment(SwingConstants.RIGHT)
      this
    }
  }
}

class DxFiler {

  import DxFiler.*

  private var currentDirectoryInfo: String = ""

  private val mainPanel: BorderPanel = new BorderPanel {
    border = new EmptyBorder(1, 1, 1, 1)
  }

  private val fileTableModel: FileTableModel = new FileTableModel

  private var treeSelectionChangeProcessing = false

  private var prevAddress: String = ""

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

  private val table: FileTable = new FileTable

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

      if (selectedFiles.isEmpty) {
        contents +=
          createMenuItem("CreateFolder", Key.F) {
            Dialog.showInput(frame, "フォルダ名", APP_NAME, initial = "").foreach { name =>
              WaitCursorWorker(frame, true) { () =>
                val parent = treePathToFile(tree.getSelectionPath)
                Dx.createFolderWithErrorMessage(new DxFile(parent, name).toString)
                refresh()
              }(null).execute()
            }
          }
      }

      if (selectedFiles.nonEmpty) {
        if (desktop.isSupported(Desktop.Action.OPEN)) {
          contents +=
            createMenuItem("Open(via Temporary)", Key.O) {
              executeAndShowError {
                selectedFiles.foreach(openFile)
              }
            }
        }
      }

      if (selectedFiles.nonEmpty) {
        contents +=
          createMenuItem("Prepare file copy", Key.F) {
            executeAndShowError {
              selectedFiles.foreach(downloadFileToTemporary)
            }
          }
      }

      if (selectedFiles.length == 1) {
        contents +=
          createMenuItem("Rename", Key.N) {
            executeAndShowError {
              renameFile(selectedFiles.head)
            }
          }
      }

      if (selectedFiles.length == 1) {
        contents +=
          createMenuItem("Replicate", Key.P) {
            executeAndShowError {
              replicateFile(selectedFiles.head)
            }
          }
      }

      if (selectedFiles.nonEmpty) {
        contents +=
          createMenuItem("Delete", Key.D) {
            executeAndShowError {
              deleteFile(selectedFiles.toArray)
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
          mnemonic =  Key.C
          contents += createMenuItem("Copy file name", Key.N)(setPropertyToClipboard(_.getName))
          contents += createMenuItem("Copy dropbox path", Key.P)(setPropertyToClipboard(_.getPath))
          contents += createMenuItem("Copy dropbox ID", Key.I)(setPropertyToClipboard(_.id))
          contents += createMenuItem("Copy properties", Key.R) {
            executeAndShowError {
              val text =
                table.selection.rows.toList.map(table.viewToModelRow).map { row =>
                  val sep = "\t"
                  "" +
                    fileTableModel.getValueAt(row, 1) + sep +
                    fileTableModel.getValueAt(row, 2) + sep +
                    fileTableModel.getValueAt(row, 3) + sep +
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

  private def openFile(file: DxFile): Unit =
    WaitCursorWorker(frame, true)(() => desktop.open(Option(file.downloaded).getOrElse(Dx.download(file))))(null).execute()

  private def downloadFileToTemporary(file: DxFile): Unit =
    WaitCursorWorker(frame, true)(() => Option(file.downloaded).getOrElse(Dx.download(file)))(null).execute()

  private def renameFile(file: DxFile, refreshParent: Boolean = false): Unit = {
    if (file.toString == "/") throw new IllegalArgumentException("can't rename root path")

    var toFile: DxFile = null

    val worker =
      WaitCursorWorker(frame, true) { () =>
        Dialog.showInput(frame, "ファイル名", APP_NAME, initial = file.getName).foreach { name =>
          toFile = new DxFile(file.getParent, name)
          Dx.renameWithErrorMessage(file.toString, toFile.toString)

          if (refreshParent) {
            tree.setSelectionPath(tree.getSelectionPath.getParentPath)
          } else {
            refresh()
          }
        }
      }(null)

    worker.execute()
  }

  private def replicateFile(file: DxFile): Unit = {
    if (file.toString == "/") throw new IllegalArgumentException("can't replicate root path")

    var toFile: DxFile = null

    val worker =
      WaitCursorWorker(frame, true) { () =>
        Dialog.showInput(frame, "ファイル名", APP_NAME, initial = file.getName).foreach { name =>
          toFile = new DxFile(file.getParent, name)
          Dx.copyWithErrorMessage(file.toString, toFile.toString)
          refresh()
        }
      }(null)

    worker.execute()
  }

  private def deleteFile(files: Array[DxFile]): Unit =
    WaitCursorWorker(frame, true) { () =>
      files.foreach { file =>
        Dx.deleteWithErrorMessage(file.toString)
      }
      refresh()
    }(null).execute()

  table.listenTo(table.mouse.clicks)
  table.listenTo(table.keys)
  table.reactions += {
    case _: TableRowsSelected =>
      val viewRows = table.selection.rows
      val model = table.model.asInstanceOf[FileTableModel]
      val count = viewRows.size
      val totalSize = viewRows.map {
        table.viewToModelRow
      }.map {
        model.getFile
      }.map {
        _.length
      }.sum
      lbStatus.text = currentDirectoryInfo + "  " + f"$count%,d items selected, $totalSize%,d bytes."
    case ev: MouseClicked if ev.clicks == 2 =>
      getSelectedFiles.foreach { file =>
        executeAndShowError {
          openOrExpandNode(file)
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
          if (file.isDirectory) {
            if (firstDirectory) {
              openOrExpandNode(file)
              firstDirectory = false
            } else {
              openFile(file)
            }
          } else {
            openOrExpandNode(file)
          }
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
    case ev: KeyPressed if ev.key == Key.F2 =>
      if (getSelectedFiles.length == 1) {
        renameFile(getSelectedFiles.head)
      }
      ev.consume()
    case ev: KeyPressed if ev.key == Key.Delete =>
      if (getSelectedFiles.nonEmpty) {
        Dialog.showConfirmation(frame, "削除しますか？", APP_NAME, Dialog.Options.OkCancel) match {
          case Dialog.Result.Ok =>
            executeAndShowError {
              deleteFile(getSelectedFiles.toArray)
            }
          case _ =>
        }
      }
  }

  private def getSelectedFiles: List[DxFile] =
    table.selection.rows.toList.map(table.viewToModelRow).map(table.model.asInstanceOf[FileTableModel].files)

  private def openOrExpandNode(file: DxFile): Unit = {
    if (file.isFile) {
      executeAndShowError {
        openFile(file)
      }
    } else {
      findTreePath(file.parentFile).foreach { treePath =>
        tree.expandPath(treePath)
        tree.setSelectionPath(findTreePath(file).orNull)
        tree.scrollPathToVisible(findTreePath(file).orNull)
      }
    }
  }

  private val treeSelectionListener: TreeSelectionListener =
    (tse: TreeSelectionEvent) => {
      if (!treeSelectionChangeProcessing) {
        try {
          treeSelectionChangeProcessing = true
          val node = tse.getPath.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
          WaitCursorWorker(frame, true)(() => refresh(node))(null).execute()
        } finally {
          treeSelectionChangeProcessing = false
        }
      }
    }

  private val treeKeyListener: KeyListener = new KeyAdapter {
    override def keyPressed(ev: KeyEvent): Unit = {
      if (ev.getKeyCode == KeyEvent.VK_F5) {
        refresh()
        ev.consume()
      } else if (ev.getKeyCode == KeyEvent.VK_F2) {
        ev.consume()
        renameFile(treePathToFile(tree.getSelectionPath), refreshParent = true)
      } else if (ev.getKeyCode == KeyEvent.VK_DELETE) {
        Dialog.showConfirmation(frame, "削除しますか？", APP_NAME, Dialog.Options.OkCancel) match {
          case Dialog.Result.Ok =>
            val file = treePathToFile(tree.getSelectionPath)
            WaitCursorWorker(frame, true) { () =>
              Dx.delete(file.toPath.toString)
              tree.setSelectionPath(tree.getSelectionPath.getParentPath)
            }(null).execute()
          case _ =>
        }
      } else if (ev.getKeyCode == KeyEvent.VK_CONTEXT_MENU) {
        ev.consume()
        val r = tree.getPathBounds(tree.getSelectionPath)
        val rx = r.x + r.width
        val ry = r.y + r.height

        showTreePopupMenu(tree, rx, ry)
      }
    }
  }

  private def showTreePopupMenu(component: JComponent, x: Int, y: Int): Unit = {
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

      contents +=
        createMenuItem("Rename", Key.N) {
          executeAndShowError {
            renameFile(treePathToFile(tree.getSelectionPath), refreshParent = true)
          }
        }

      contents +=
        createMenuItem("Delete", Key.D) {
          val file = treePathToFile(tree.getSelectionPath)
          WaitCursorWorker(frame, true) { () =>
            Dx.delete(file.toPath.toString)
            tree.setSelectionPath(tree.getSelectionPath.getParentPath)
          }(null).execute()
        }
    }.show(Component.wrap(component), x, y)
  }

  private def getChildren(file: DxFile): Array[DxFile] =
    file.listFiles.sortWith {
      case (f1: File, f2: File) =>
        if (f1.isDirectory && f2.isFile) true else if (f1.isFile && f2.isDirectory) false else f1.getName < f2.getName
    }.asInstanceOf[Array[DxFile]]

  private def getDirectoryChildren(file: DxFile): Array[DxFile] =
    getChildren(file).filter {
      _.isDirectory
    }

  private val rootTreeNode: DefaultMutableTreeNode =
    new DefaultMutableTreeNode {
      private val fileSystemRoot = DxRootPath.toDxFile

      val node = new DefaultMutableTreeNode(fileSystemRoot)

      add(node)

      getDirectoryChildren(fileSystemRoot).map {
        new DefaultMutableTreeNode(_)
      }.foreach {
        node.add
      }
    }

  private val tree: JTree = new JTree(new DefaultTreeModel(rootTreeNode)) {
    override def scrollRectToVisible(rect: Rectangle): Unit = {
      super.scrollRectToVisible(new Rectangle(0, 0 max (rect.y - rect.height), rect.width, getHeight min rect.height * 3))
    }

    setRootVisible(false)
    addTreeSelectionListener(treeSelectionListener)
    addKeyListener(treeKeyListener)
    setCellRenderer(new FileTreeCellRenderer(getCellRenderer))
    expandRow(0)
    setVisibleRowCount(15)

    addMouseListener(new MouseAdapter {
      override def mouseClicked(ev: MouseEvent): Unit = {
        if (ev.getClickCount == 1 && ev.getModifiersEx == InputEvent.META_DOWN_MASK) {
          ev.consume()
          val treePath = getPathForLocation(ev.getX, ev.getY)
          if (treePath != null) {
            if (getSelectionPaths == null || !getSelectionPaths.contains(treePath)) tree.setSelectionPath(treePath)
            showTreePopupMenu(tree, ev.getX, ev.getY)
          }
        }
      }
    })
  }

  private def executeAndShowError(f: => Unit): Unit = {
    try {
      f
    } catch {
      case t: Throwable =>
        showError("エラーが発生しました。", t)
    }

    mainPanel.repaint()
  }

  private val treeScroll: ScrollPane = new ScrollPane(Component wrap tree) {
    preferredSize = new Dimension(200, preferredSize.getHeight.toInt)
  }

  private val lbStatus: Label = new Label("") {
    border = new EmptyBorder(1, 1, 1, 1)
    horizontalAlignment = Alignment.Leading
  }

  private val tableScroll: ScrollPane = new ScrollPane(table) {
    preferredSize = new Dimension(1000, preferredSize.getHeight.toInt)

    listenTo(mouse.clicks, horizontalScrollBar.mouse.clicks, verticalScrollBar.mouse.clicks)
    reactions += {
      case ev@MouseClicked(source, point, Key.Modifier.Meta, 1, _) =>
        ev.consume()
        table.selection.rows.clear()
        showTablePopupMenu(source, point.x, point.y)
    }
  }

  private val tablePanel: BorderPanel = new BorderPanel {
    layout(tableScroll) = Center
    layout(new BorderPanel {
      layout(lbStatus) = Center
      layout(WaitCursorWorker.progressBar) = East
    }) = South
  }

  private val tfAddress: TextField = new TextField

  private def jumpToAddress(): Unit = boundary {

    if (treeSelectionChangeProcessing) break()
    if (prevAddress == tfAddress.text) break()

    WaitCursorWorker(frame, true)(() => {
      var address = tfAddress.text
      address += (if address.endsWith("/") then "" else "/")

      val addressList = mutable.ArrayBuffer[String]("/")
      var pos = 0
      while ( {
        pos = address.indexOf("/", pos + 1)
        pos >= 0
      }) {
        addressList.addOne(address.substring(0, pos))
      }

      var lastFoundFile: DxFile = null

      addressList.foreach { path =>
        val file = new DxFile(new DxPath(path, DxFileAttributes(true, null, 0, null)))

        findTreePath(file).foreach { treePath =>

          val node = treePath.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
          node.removeAllChildren()

          val children = getChildren(file)
          children.filter(_.isDirectory).foreach { child =>
            node.add(new DefaultMutableTreeNode(child))
          }
          setTableData(children)
          setCurrentDirectoryInfo(file, children)

          tree.getModel.asInstanceOf[DefaultTreeModel].reload(node)

          tree.expandPath(treePath)

          lastFoundFile = file
        }
      }

      if (lastFoundFile != null) {
        tree.setSelectionPath(findTreePath(lastFoundFile).orNull)
        tree.scrollPathToVisible(findTreePath(lastFoundFile).orNull)
      }

    })(null).execute()
  }

  tfAddress.listenTo(tfAddress, tfAddress.keys)
  tfAddress.reactions += {
    case ev: KeyPressed if ev.key == Key.Enter =>
      jumpToAddress()
    case ev: FocusLost =>
      jumpToAddress()
  }

  tree.setSelectionInterval(0, 0)

  mainPanel.layout(tfAddress) = North
  mainPanel.layout(new SplitPane(Vertical, treeScroll, tablePanel)) = Center

  private def findTreePath(target: DxFile): Option[TreePath] = {
    (0 until tree.getRowCount).map {
      tree.getPathForRow
    }.find {
      _.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode].getUserObject.asInstanceOf[DxFile] == target
    }
  }

  private def setTableData(files: Array[DxFile]): Unit = {
    table.deafTo(table.selection)
    fileTableModel.setFiles(files)
    table.listenTo(table.selection)
  }

  private def refresh(node: DefaultMutableTreeNode = treePathToNode(tree.getSelectionPaths.head)): Unit = {
    val file = node.getUserObject.asInstanceOf[DxFile]

    node.removeAllChildren()

    val worker = new SwingWorker[Unit, File] {
      override def doInBackground(): Unit = {
        val children = getChildren(file)
        children.filter(_.isDirectory).foreach(publish(_))
        setTableData(children)
        setCurrentDirectoryInfo(file, children)
      }

      override protected def process(children: util.List[File]): Unit = {
        children.asScala.foreach { child =>
          node.add(new DefaultMutableTreeNode(child))
        }
      }

      override protected def done(): Unit = {
        tree.getModel.asInstanceOf[DefaultTreeModel].reload(node)
      }
    }

    worker.execute()
    worker.get
  }

  private def setCurrentDirectoryInfo(file: File, children: Array[DxFile]): Unit = {
    val count = children.length
    val totalSize = children.map(_.length).sum
    currentDirectoryInfo = f"$count%,d items, $totalSize%,d bytes."
    lbStatus.text = currentDirectoryInfo

    tfAddress.text = file.getPath
    prevAddress = file.getPath

    mainPanel.peer.getTopLevelAncestor match {
      case f: JFrame => f.setTitle(APP_NAME + " : " + file.getName)
      case _ =>
    }

    mainPanel.repaint()
  }

  private object FileTableModel {

    private case class ColumnInfo(name: String, clazz: Class[_], minChars: Int = -1, maxChars: Int = -1, preferredChars: Int = -1)

    private val columns: Array[ColumnInfo] = {
      Array(
        ColumnInfo("Icon", classOf[ImageIcon], 4, 4, 4),
        ColumnInfo("File", classOf[String], -1, -1, 60),
        ColumnInfo("Ext", classOf[String], -1, -1, 8),
        ColumnInfo("Size", classOf[java.lang.Long], -1, -1, 13),
        ColumnInfo("Last Modified", classOf[String], 19, 19, 19),
        ColumnInfo("ID", classOf[String], -1, -1, 26),
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
        case "File" => file.getName
        case "Ext" => if (file.getName.contains(".")) file.getName.substring(file.getName.lastIndexOf(".") + 1) else ""
        case "Size" => if (file.isDirectory) null else java.lang.Long.valueOf(file.length)
        case "Last Modified" => if (file.lastModified <= 0) null else new SimpleDateFormat("yyyy/MM/dd HH:mm:ss").format(new Date(file.lastModified))
        case "ID" => file.id
      }
    }

    override def getRowCount: Int = files.length

    def getFile(row: Int): File = files(row)

    def setFiles(files: Array[DxFile]): Unit = {
      this.files = files
      fireTableDataChanged()
    }
  }

  private class FileTreeCellRenderer(originalRenderer: TreeCellRenderer) extends DefaultTreeCellRenderer {

    override def getTreeCellRendererComponent(tree: JTree, value: Any, selected: Boolean, expanded: Boolean, leaf: Boolean, row: Int, hasFocus: Boolean): AComponent = {
      super.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus)

      val file = value.asInstanceOf[DefaultMutableTreeNode].getUserObject.asInstanceOf[DxFile]

      setIcon(DIRECTORY_ICON)

      if (file != null) {
        setText(file.getName)
        setToolTipText(file.getPath)
      }

      this
    }
  }

  private def treePathToNode: TreePath => DefaultMutableTreeNode = Option(_).map(_.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]).orNull

  private def nodeToFile: DefaultMutableTreeNode => DxFile = Option(_).map(_.getUserObject.asInstanceOf[DxFile]).orNull

  private def treePathToFile: TreePath => DxFile = treePathToNode andThen nodeToFile

  private def treePathsToNodes: scala.collection.Seq[TreePath] => scala.collection.Seq[DefaultMutableTreeNode] = Option(_).map(_.map(treePathToNode)).getOrElse(Nil)

  private def nodesToFiles: scala.collection.Seq[DefaultMutableTreeNode] => scala.collection.Seq[DxFile] = Option(_).map(_.map(nodeToFile)).getOrElse(Nil)

  private def treePathsToFiles: scala.collection.Seq[TreePath] => scala.collection.Seq[DxFile] = treePathsToNodes andThen nodesToFiles

  private class DxFileTransferHandler extends TransferHandler {

    private var inQuestionDialog: Boolean = false

    override protected def createTransferable(c: JComponent): Transferable = {
      val supportFlavors = Array[DataFlavor](DxFileListFlavor, DataFlavor.javaFileListFlavor)

      val fileList: Array[DxFile] =
        c match {
          case _: JTable | _: JScrollPane =>
            table.selection.rows.map(table.viewToModelRow).map(fileTableModel.files(_)).toArray
          case _: JTree =>
            treePathsToFiles(tree.getSelectionPaths).toArray
        }

      class FileTransferable extends Transferable {
        override def getTransferDataFlavors: Array[DataFlavor] = supportFlavors

        override def isDataFlavorSupported(flavor: DataFlavor): Boolean = supportFlavors.contains(flavor)

        override def getTransferData(flavor: DataFlavor): Object = {
          if (flavor == DxFileListFlavor) {
            new DxFileList(util.Arrays.asList(fileList: _*))
          } else if (flavor == DataFlavor.javaFileListFlavor) {
            if (fileList.exists(_.downloaded == null)) {
              if (!inQuestionDialog) {
                inQuestionDialog = true
                Future {
                  Dialog.showConfirmation(frame, "コピー準備のために一時領域にダウンロードしますか？", APP_NAME, Dialog.Options.OkCancel) match {
                    case Dialog.Result.Ok =>
                      fileList.foreach { f => downloadFileToTemporary(f) }
                    case _ =>
                      exportDone(c, null, DnDConstants.ACTION_NONE)
                  }
                  inQuestionDialog = false
                }
              }

              null
            } else {
              util.Arrays.asList(fileList.map {
                case file: DxFile => Option(file.downloaded).getOrElse(Dx.download(file))
              }: _*)
            }
          } else {
            throw new UnsupportedFlavorException(flavor)
          }
        }
      }

      new FileTransferable
    }

    override def canImport(info: TransferHandler.TransferSupport): Boolean = {
      val flavor = List(DxFileListFlavor, DataFlavor.javaFileListFlavor).find(info.isDataFlavorSupported).orNull

      info.setDropAction(TransferHandler.COPY)

      if (!info.isDrop || flavor == null) {
        false
      } else {
        val isSourceDxFiles = info.isDataFlavorSupported(DxFileListFlavor)

        def canImportDxFile(destFile: DxFile): Boolean = {
          val destPath = destFile.toString

          val sourceFiles = info.getTransferable.getTransferData(flavor).asInstanceOf[util.List[DxFile]].asScala
          val sourcePaths = sourceFiles.map(_.toString)
          val sourceDirPaths = sourceFiles.filter(_.isDirectory).map(_.toString).map(s => if s == "/" then "/" else s + "/")
          val sourceParentPaths = sourceFiles.map(_.parentFile).filter(_ != null).map(_.toString)

          destFile.isDirectory && !sourcePaths.contains(destPath) && !sourceDirPaths.exists(destPath.startsWith) && !sourceParentPaths.contains(destPath)
        }

        info.getComponent match {
          case _: JTable | _: JScrollPane =>
            if (isSourceDxFiles) {
              val index =
                info.getDropLocation match {
                  case loc: JTable.DropLocation => table.viewToModelRow(loc.getRow)
                  case _ => -1
                }
              val destFile = if (index >= 0) fileTableModel.files(index) else treePathToFile(tree.getSelectionPath)
              canImportDxFile(destFile)
            } else true
          case _: JTree =>
            val treePath = info.getDropLocation.asInstanceOf[JTree.DropLocation].getPath
            if (treePath == null) {
              false
            } else if (isSourceDxFiles) {
              canImportDxFile(treePathToFile(treePath))
            } else true
        }
      }
    }

    override def getSourceActions(c: JComponent): Int = TransferHandler.COPY

    override def importData(info: TransferHandler.TransferSupport): Boolean = {
      if (!canImport(info)) return false

      val flavor = List(DxFileListFlavor, DataFlavor.javaFileListFlavor).find(info.isDataFlavorSupported).orNull

      if (flavor == DxFileListFlavor) {
        copyDroppedFiles(info, flavor)
      } else {
        uploadDroppedFiles(info, flavor)
      }
    }

    private def uploadDroppedFiles(info: TransferHandler.TransferSupport, flavor: DataFlavor): Boolean = {
      val files: util.List[File] =
        Try(info.getTransferable.getTransferData(flavor).asInstanceOf[util.List[File]]) match {
          case Success(f: util.List[File]) =>
            f
          case Failure(ex) =>
            Future {
              showWarning("ファイルの転送がキャンセルされました。", ex)
            }
            return false
        }

      Try {
        val destDir =
          info.getComponent match {
            case _: JTable | _: JScrollPane =>
              Try(fileTableModel.files(table.viewToModelRow(info.getDropLocation.asInstanceOf[JTable.DropLocation].getRow))).getOrElse(treePathToFile(tree.getSelectionPath))
            case _: JTree =>
              treePathToFile(info.getDropLocation.asInstanceOf[JTree.DropLocation].getPath)
          }

        WaitCursorWorker(frame, true) { () =>
          Dx.upload(destDir, files.toArray(Array[File]()))
          refresh()
        }(null).execute()

      } match {
        case Success(_) =>
          true
        case Failure(ex) =>
          ex.printStackTrace()
          showError("アップロードに失敗しました。", ex)
          false
      }
    }

    private def copyDroppedFiles(info: TransferHandler.TransferSupport, flavor: DataFlavor): Boolean = {
      val files: util.List[DxFile] =
        Try(info.getTransferable.getTransferData(flavor).asInstanceOf[util.List[DxFile]]) match {
          case Success(f: util.List[DxFile]) =>
            f
          case Failure(ex) =>
            showError("ファイルの取得に失敗しました。", ex)
            return false
        }

      Try {
        val destDir =
          info.getComponent match {
            case _: JTable | _: JScrollPane =>
              Try(fileTableModel.files(table.viewToModelRow(info.getDropLocation.asInstanceOf[JTable.DropLocation].getRow))).getOrElse(treePathToFile(tree.getSelectionPath))
            case _: JTree =>
              treePathToFile(info.getDropLocation.asInstanceOf[JTree.DropLocation].getPath)
          }

        WaitCursorWorker(frame, true) { () =>
          files.asScala.foreach { file =>
            Dx.copy(file.toString, new DxFile(destDir, file).toString) match {
              case Right(_) =>
              case Left(result) =>
                System.err.println(result)
                showError("コピーに失敗しました。", result)
            }
          }
          refresh()
        }(null).execute()

      } match {
        case Success(_) =>
          true
        case Failure(ex) =>
          ex.printStackTrace()
          showError("コピーに失敗しました。", ex)
          false
      }
    }

    override def exportAsDrag(comp: JComponent, e: InputEvent, action: Int): Unit = {
      comp match {
        case _: JTable | _: JScrollPane =>
          super.exportAsDrag(comp, e, action)
        case _: JTree =>
          if (treePathToFile(tree.getSelectionPath).toString != "/") super.exportAsDrag(comp, e, action)
      }
    }
  }

  private val handler = new DxFileTransferHandler

  table.peer.setTransferHandler(handler)
  table.peer.setDropMode(DropMode.ON)
  table.peer.setDragEnabled(true)

  tableScroll.peer.setTransferHandler(handler)

  tree.setTransferHandler(handler)
  tree.setDropMode(DropMode.ON)
  tree.setDragEnabled(true)
}
