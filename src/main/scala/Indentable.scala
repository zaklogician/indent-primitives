package indentor;

sealed trait Indentable {
  def ++(that: Indentable) = Indentable.Concat(this,that)

  def ++||(that: Indentable) = this ++ Indentable.Literal(" ") ++ Indentable.SoftBreak ++ that

  def ++|(that: Indentable) = this ++ Indentable.Literal(" ") ++ that

  def >>(that: Indentable) = this ++ Indentable.WithCurrentColumn { c =>
    Indentable.SetLevel(c, that)
  }

  def >>||(that: Indentable) = this ++|| Indentable.WithCurrentColumn { c =>
    Indentable.SetLevel(c, that)
  }
  
  def >>|(that: Indentable) = this ++| Indentable.WithCurrentColumn { c =>
    Indentable.SetLevel(c, that)
  }

  def <<(that: Indentable) = Indentable.StrainToRightBegin(this, that)
  /* The external API should probably consist of these operations, along with
   * HardBreak, AddLevel and SetLevel; these operations can be given
   * well-defined semantics independently of the renderer implementation. */
}

object Indentable {
  case object Empty extends Indentable
  case object SoftBreak extends Indentable // if the line is too long, do a line break
  case object HardBreak extends Indentable // always do a line break
  case class Literal(text: String) extends Indentable
  case class AddLevel(level: Int, body: Indentable) extends Indentable
  case class SetLevel(level: Int, body: Indentable) extends Indentable
  case class Concat(left: Indentable, right: Indentable) extends Indentable
  case class WithCurrentColumn(body: Int => Indentable) extends Indentable
  case class WithCurrentLevel(body: Int => Indentable) extends Indentable
  case class StrainToRightBegin(left: Indentable, right: Indentable) extends Indentable

  def Text(from: String): Indentable = from.split("\n").toList match {
    case Nil => Empty
    case (x :: xs) =>
      xs.foldLeft(Literal(x).asInstanceOf[Indentable])(_ ++ HardBreak ++ Literal(_))
  }
}

