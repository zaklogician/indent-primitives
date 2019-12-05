package indentor;

object TestObject {
  def name: String = "TestObject"
}

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
}

sealed trait Indented {
  def toText: String
}
object Indented {
  case object Empty extends Indented {
    override def toText: String = ""
  }
  case class Literal(text: String, rest: Indented) extends Indented {
    override def toText: String = text + rest.toText
  }
  case class Newline(level: Int, body: Indented) extends Indented {
    override def toText: String = "\n" + List.fill(level)(' ').mkString + body.toText
  }
}

object Render {

  val debug = false
  def debugPrint(str: String): Unit = {
    if (debug) println(str)
  }

  case class State(currentLevel: Int, target: Indentable)
  type Pipeline = List[State]

  def render(width: Int, currentColumn: Int, pipeline: Pipeline): Indented = pipeline match {
    case Nil => Indented.Empty
    case (State(currentLevel,target) :: rest) => target match {
      case Indentable.Empty => Indented.Empty
      case Indentable.SoftBreak => {
        debugPrint("In column " + currentColumn + " break over width " + width)
        if (currentColumn > width)
           render(width, currentColumn, State(currentLevel,Indentable.HardBreak) :: rest)
        else render(width, currentColumn, rest)
      }
      case Indentable.HardBreak => {
        debugPrint("Rendering newline")
        Indented.Newline(currentLevel, render(width, currentLevel, rest))
      }
      case Indentable.Literal(text) => {
        debugPrint("In column " + currentColumn + " rendering literal '" + text ++ "'")
        Indented.Literal(text, render(width, currentColumn + text.length, rest))
      }
      case Indentable.AddLevel(level, body) => {
        render(width, currentColumn, State(currentLevel + level, body) :: rest)
      }
      case Indentable.SetLevel(level, body) => {
        render(width, currentColumn, State(level, body) :: rest)
      }
      case Indentable.Concat(left, right) => {
        render(width, currentColumn, State(currentLevel,left) :: State(currentLevel,right) :: rest )
      }
      case Indentable.WithCurrentColumn(body) => {
        debugPrint("Call with current column " + currentColumn)
        render(width, currentColumn, State(currentLevel, body(currentColumn)) :: rest)
      }
      case Indentable.WithCurrentLevel(body) => {
        render(width, currentColumn, State(currentLevel, body(currentLevel)) :: rest)
      }
      case Indentable.StrainToRightBegin(left, right) => {
        render(width, currentColumn, State(currentLevel, right) :: rest) match {
          case Indented.Newline(level,_) => render(
            width,
            currentColumn,
            State(level, left) :: State(currentLevel, right) :: rest
          )
          case _ => render(
            width,
            currentColumn,
            State(currentLevel, left) :: State(currentLevel, right) :: rest
          )
        } // end render(..) match
      } // end target match
    } // end pipeline match
  } // end def render

  def apply(target: Indentable, width: Int): Indented = render(width,0,List(State(0,target)))

  def apply(target: Indentable): Indented = render(80,0,List(State(0,target)))

}
