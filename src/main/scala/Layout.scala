package indentor;

/** A trait for rendered, explicitly indented pieces of text.
  *
  * This trait comes with three implementing case classes, `indentor.Layout.Empty`,
  * `indentor.Layout.Literal` and `indentor.Layout.Newline` which implement 
  * abstract members for displaying the data.
  *
  * Instances of `Layout` have an explicitly defined layout, meaning that there is only one
  * correct way of displaying them as a `String`.
  */
sealed trait Layout {
  /** Displays this `Layout` instance as a single `String`. */
  def asString: String
}

object Layout {

  case object Empty extends Layout {
    override val asString: String = ""
  }

  case class Literal(head: String, tail: Layout) extends Layout {
    override lazy val asString: String = head + tail.asString
  }

  case class Newline(level: Int, body: Layout) extends Layout {
    override lazy val asString: String = "\n" + List.fill(level)(' ').mkString + body.asString
  }

}

