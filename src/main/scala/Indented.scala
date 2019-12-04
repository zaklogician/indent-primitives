package indentor;

/** A trait for rendered, explicitly indented pieces of text.
  *
  * This trait comes with three implementing case classes, `indentor.Indented.Empty`,
  * `indentor.Indented.Literal` and `indentor.Indented.Newline` which implement 
  * abstract members for displaying the data.
  *
  * Instances of `Indented` have an explicitly defined layout, meaning that there is only one
  * correct way of displaying them as a `String`.
  */
sealed trait Indented {
  /** Displays this `Indented` instance as a single `String`. */
  def asString: String
}

object Indented {

  /** A case object that has the empty string as its string representation. */
  case object Empty extends Indented {
    override val asString: String = ""
  }

  case class Literal(head: String, tail: Indented) extends Indented {
    override lazy val asString: String = head + tail.asString
  }

  case class Newline(level: Int, body: Indented) extends Indented {
    override lazy val asString: String = "\n" + List.fill(level)(' ').mkString + body.asString
  }
}

