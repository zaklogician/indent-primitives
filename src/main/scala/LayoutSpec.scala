package indentor;

/** A trait for pieces of text equipped with an indentation specification.
  *
  * Instances of `LayoutSpec` do not specify a layout explicitly: they should
  * be thought of as representing "sets of possible `Layout`s" for a given
  * piece of text. The `indentor.Render` object can be used to find an explicit
  * `indentor.Layout` satisfying the specification. The resulting `Layout` can
  * then be displayed as a `String.
  */
sealed trait LayoutSpec {
  /** Returns true if the given `LayoutSpec` can be rendered in forward mode,
    * meaning that it does not contain `SetLevel`s, negative `AddLevel`s or
    * left strains.
    */
  val isForward: Boolean
    
  /** Constructs a `LayoutSpec` that is satisfied if all the rendered literals
    * of the given `LayoutSpec` starts to the right of or below those of
    * `this` LayoutSpec.
    */
  def ++(that: LayoutSpec) = LayoutSpec.Concat(this,that)


  /** Constructs a `LayoutSpec` that is satisfied if all the forward-mode renderable
   *  literals of the given `LayoutSpec` starts to the right of the last such literal
   *  of `this` one.
   */
  def >>(that: LayoutSpec) = this ++ LayoutSpec.WithCurrentColumn { c =>
    LayoutSpec.SetLevel(c, that)
  }

  /** Constructs a `LayoutSpec` that is satisfied if the last forward-mode renderable
    * line of `this` starts to the right of the first such line of the given `LayoutSpec`.
    */
  def <<(that: LayoutSpec) = LayoutSpec.StrainToRightBegin(this, that)

  
  def ++||(that: LayoutSpec) = this ++ LayoutSpec.Literal(" ") ++ LayoutSpec.SoftBreak ++ that

  def ++|(that: LayoutSpec) = this ++ LayoutSpec.Literal(" ") ++ that

  def >>||(that: LayoutSpec) = this ++|| LayoutSpec.WithCurrentColumn { c =>
    LayoutSpec.SetLevel(c, that)
  }
  
  def >>|(that: LayoutSpec) = this ++| LayoutSpec.WithCurrentColumn { c =>
    LayoutSpec.SetLevel(c, that)
  }

}

object LayoutSpec {


  /** Constructs a `LayoutSpec` representing `Layout.Empty`. */
  case object Empty extends LayoutSpec {
    override val isForward: Boolean = true
  }

  /** Constructs a `LayoutSpec` that is satisfied if rendered as a line break, or
    * if not rendered at all.
    *
    * Generally, if a line is too long, then the renderer will insert a break here.
    */ 
  case object SoftBreak extends LayoutSpec {
    override val isForward: Boolean = true
  }

  /** Constructs a `LayoutSpec` representing a `Layout.Newline`. */
  case object HardBreak extends LayoutSpec {
    override val isForward: Boolean = true
  }

  /** Constructs a `LayoutSpec` from the given literal `String`, replacing newline
    * characters with `LayoutSpec.HardBreak`s. */
  def TextLiteral(from: String): LayoutSpec = from.split("\n").toList match {
    case Nil => Empty
    case (x :: xs) =>
      xs.foldLeft(Literal(x).asInstanceOf[LayoutSpec])(_ ++ HardBreak ++ Literal(_))
  }

  /** Constructs a `LayoutSpec` from the given `LayoutSpec` such that
    * all the forward-mode literals that start at column C in the given `LayoutSpec`
    * start at column C+`level` in the result.
    */
  case class AddLevel(level: Int, body: LayoutSpec) extends LayoutSpec {
    override val isForward: Boolean = (level >= 0) && body.isForward
  }


  // deep API: //
  
  /** Constructs a `LayoutSpec` from the given `LayoutSpec` such that the leftmost
    * forward-mode line of the given `LayoutSpec` starts at the given column `level`
    * in the result.
    */
  case class SetLevel(level: Int, body: LayoutSpec) extends LayoutSpec {
    override val isForward: Boolean = false
  }

  case class Literal(text: String) extends LayoutSpec {
    override val isForward: Boolean = true
    require( !text.contains("\n") )
  }
  case class Concat(left: LayoutSpec, right: LayoutSpec) extends LayoutSpec {
    override val isForward: Boolean = left.isForward && right.isForward
  }
  case class WithCurrentColumn(body: Int => LayoutSpec) extends LayoutSpec {
    override val isForward: Boolean = body(0).isForward
  }
  case class WithCurrentLevel(body: Int => LayoutSpec) extends LayoutSpec {
    override val isForward: Boolean = body(0).isForward
  }
  case class StrainToRightBegin(left: LayoutSpec, right: LayoutSpec) extends LayoutSpec {
    override val isForward: Boolean = false
  }

}

