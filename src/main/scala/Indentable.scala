package indentor;

object TestObject {
  def name: String = "TestObject"
}

sealed trait Indentable {
  def <>(that: Indentable) = Indentable.Concat(this,that)

  def <|>(that: Indentable) = this <> Indentable.SoftBreak <> that
  
  def aligned: Indentable = Indentable.WithCurrentColumn { c =>
    Indentable.WithCurrentLevel { l =>
      Indentable.AddLevel(c - l, this)
    }
  }

  def <<>(that: Indentable) = (this <> that).aligned

  def <<|>(that: Indentable) = (this <|> that).aligned

  def <>>(that: Indentable) = Indentable.StrainToRightBegin(this, that)
}
object Indentable {
  case object Empty extends Indentable
  case object SoftBreak extends Indentable
  case object HardBreak extends Indentable
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

  case class State(currentLevel: Int, target: Indentable)
  type Pipeline = List[State]

  def canFitIn(width: Int, currentColumn: Int, pipeline: Pipeline): Boolean =
    if (width < currentColumn) false else pipeline match {
      case Nil => true
      case (State(currentLevel,target) :: rest) => target match {
        case Indentable.Empty =>
          canFitIn(width, currentColumn, rest)
        case Indentable.SoftBreak =>
          canFitIn(width, currentColumn, rest)
        case Indentable.HardBreak =>
          true
        case Indentable.Literal(text) =>
          canFitIn(width, currentColumn + text.length, rest)
        case Indentable.AddLevel(level, body) =>
          canFitIn(width, currentColumn, State(currentLevel + level, body) :: rest)
        case Indentable.SetLevel(level, body) =>
          canFitIn(width, currentColumn, State(level, body) :: rest)
        case Indentable.Concat(left, right) =>
          canFitIn(
            width,
            currentColumn,
            State(currentLevel, left) :: State(currentLevel, right) :: rest
          )
        case Indentable.WithCurrentColumn(body) =>
          canFitIn(width, currentColumn, State(currentLevel, body(currentColumn)) :: rest)
        case Indentable.WithCurrentLevel(body) =>
          canFitIn(width, currentColumn, State(currentLevel, body(currentLevel)) :: rest)
        case Indentable.StrainToRightBegin(left, right) =>
          canFitIn(
            width,
            currentColumn,
            State(currentLevel, left) :: State(currentLevel, right) :: rest
          )
      }
    }

  def render(width: Int, currentColumn: Int, pipeline: Pipeline): Indented = pipeline match {
    case Nil => Indented.Empty
    case (State(currentLevel,target) :: rest) => target match {
      case Indentable.Empty => Indented.Empty
      case Indentable.SoftBreak =>
        if (canFitIn(width,currentColumn,pipeline)) render(
          width,
          currentColumn,
          State(currentLevel, Indentable.Literal(" ")) :: rest
        ) else Indented.Newline(currentLevel, render(width, currentLevel, rest))
      case Indentable.HardBreak =>
        Indented.Newline(currentLevel, render(width, currentLevel, rest))
      case Indentable.Literal(text) =>
        Indented.Literal(text, render(width, currentColumn + text.length, rest))
      case Indentable.AddLevel(level, body) =>
        render(width, currentColumn, State(currentLevel + level, body) :: rest)
      case Indentable.SetLevel(level, body) =>
        render(width, currentColumn, State(level, body) :: rest)
      case Indentable.Concat(left, right) =>
        render(width, currentColumn, State(currentLevel, left) :: State(currentLevel, right) :: rest )
      case Indentable.WithCurrentColumn(body) =>
        render(width, currentColumn, State(currentLevel, body(currentColumn)) :: rest)
      case Indentable.WithCurrentLevel(body) =>
        render(width, currentColumn, State(currentLevel, body(currentLevel)) :: rest)
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

  def apply(target: Indentable): Indented = render(80,0,List(State(0,target)))

}
