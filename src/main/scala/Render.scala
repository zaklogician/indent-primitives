package indentor;


/** An object with members for rendering implicit-layout instances of `indentor.Indentable`
  * into explicit-layout instances of `indentor.Indented`.
  *
  * Instances of the `indentor.Indentable` trait do not determine a single, unique layout
  * of their contents. They merely express a specification that any acceptable layout of their
  * contents must satisfy.
  *
  * Rendering is the process of finding a layout that meets the constraints of a given
  * specification. A rendering function must generate a valid layout, but may employ
  * heuristics to satisfy additional desiderata, taking into account aesthetic or pragmatic
  * considerations.
  */
object Render {

  case class State(currentLevel: Int, target: Indentable)
  type Pipeline = List[State]

  def render(width: Int, currentColumn: Int, pipeline: Pipeline): Indented = pipeline match {
    case Nil => Indented.Empty
    case (State(currentLevel,target) :: rest) => target match {
      case Indentable.Empty => Indented.Empty
      case Indentable.SoftBreak => {
        if (currentColumn > width)
           render(width, currentColumn, State(currentLevel,Indentable.HardBreak) :: rest)
        else render(width, currentColumn, rest)
      }
      case Indentable.HardBreak => {
        Indented.Newline(currentLevel, render(width, currentLevel, rest))
      }
      case Indentable.Literal(text) => {
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

  /** Renders the given `Indentable`, rendering soft breaks as newlines whenever
    * the length of the line exceeds the given `width`.
    */
  def apply(target: Indentable, width: Int): Indented = render(width,0,List(State(0,target)))

  /** Renders the given `Indentable`, rendering soft breaks as newlines whenever
    * the length of the line exceeds 80 characters.
    */
  def apply(target: Indentable): Indented = render(80,0,List(State(0,target)))

}
