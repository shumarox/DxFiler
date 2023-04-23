package ice

import java.awt.*
import javax.swing.{JPanel, JRootPane}
import scala.swing.{Frame, ProgressBar}

object WaitCursorWorker {
  def apply(frm: Frame, resumeFocus: Boolean)(doInBackgroundImpl: () => Unit)(doneImpl: () => Unit) =
    new WaitCursorWorker(frm.peer.getRootPane, doInBackgroundImpl, doneImpl, resumeFocus = resumeFocus)

  val progressBar: ProgressBar = new ProgressBar {
    visible = false
  }

  val glassPane: JPanel = {
    new JPanel {
      def setWaitCursor(): Unit = setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR))

      override def paint(g: Graphics): Unit = {
        val g2 = g.create.asInstanceOf[Graphics2D]
        g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.07f))
        g2.setPaint(Color.BLACK)
        g2.fillRect(0, 0, getWidth, getHeight)
        g2.dispose()

        super.paint(g)
      }

      setOpaque(false)

      setFocusTraversalPolicy(new DefaultFocusTraversalPolicy {
        override def accept(c: Component) = false
      })

      requestFocusInWindow
      setWaitCursor()

      override def setVisible(flag: Boolean): Unit = {
        super.setVisible(flag)
        setFocusTraversalPolicyProvider(flag)
      }
    }
  }
}

class WaitCursorWorker(rootPane: JRootPane, doInBackgroundImpl: () => Unit, doneImpl: () => Unit, resumeFocus: Boolean)
  extends javax.swing.SwingWorker[Object, Object] {

  import WaitCursorWorker.*

  override final def doInBackground(): Object = {
    val focusComponent = KeyboardFocusManager.getCurrentKeyboardFocusManager.getFocusOwner

    rootPane.setGlassPane(glassPane)
    rootPane.getGlassPane.setVisible(true)
    progressBar.visible = true
    progressBar.indeterminate = true

    try {
      doInBackgroundImpl()
    } catch {
      case ex: Throwable => ex.printStackTrace()
    } finally {
      if (resumeFocus) focusComponent.requestFocus()
    }

    "Done"
  }

  override final def done(): Unit = {
    try {
      if (doneImpl != null) doneImpl()
    } finally {
      progressBar.indeterminate = false
      progressBar.visible = false
      rootPane.getGlassPane.setVisible(false)
    }
  }
}
