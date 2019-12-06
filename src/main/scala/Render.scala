package indentor;


/** An object with members for rendering implicit-layout instances of `indentor.LayoutSpec`
  * into explicit-layout instances of `indentor.Layout`.
  *
  * Instances of the `indentor.LayoutSpec` trait do not determine a single, unique layout
  * of their contents. They merely express a specification that any acceptable layout of their
  * contents must satisfy.
  *
  * Rendering is the process of finding a layout that meets the constraints of a given
  * specification. A rendering function must generate a valid layout, but may employ
  * heuristics to satisfy additional desiderata, taking into account aesthetic or pragmatic
  * considerations.
  */
object Render {

  case class State(currentLevel: Int, target: LayoutSpec)
  type Pipeline = List[State]

  def render(width: Int, currentColumn: Int, pipeline: Pipeline): Layout = pipeline match {
    case Nil => Layout.Empty
    case (State(currentLevel,target) :: rest) => target match {
      case LayoutSpec.Empty => Layout.Empty
      case LayoutSpec.SoftBreak => {
        if (currentColumn > width)
           render(width, currentColumn, State(currentLevel,LayoutSpec.HardBreak) :: rest)
        else render(width, currentColumn, rest)
      }
      case LayoutSpec.HardBreak => {
        Layout.Newline(currentLevel, render(width, currentLevel, rest))
      }
      case LayoutSpec.Literal(text) => {
        Layout.Literal(text, render(width, currentColumn + text.length, rest))
      }
      case LayoutSpec.AddLevel(level, body) => {
        render(width, currentColumn, State(currentLevel + level, body) :: rest)
      }
      case LayoutSpec.SetLevel(level, body) => {
        render(width, currentColumn, State(level, body) :: rest)
      }
      case LayoutSpec.Concat(left, right) => {
        render(width, currentColumn, State(currentLevel,left) :: State(currentLevel,right) :: rest )
      }
      case LayoutSpec.WithCurrentColumn(body) => {
        render(width, currentColumn, State(currentLevel, body(currentColumn)) :: rest)
      }
      case LayoutSpec.WithCurrentLevel(body) => {
        render(width, currentColumn, State(currentLevel, body(currentLevel)) :: rest)
      }
      case LayoutSpec.StrainToRightBegin(left, right) => {
        render(width, currentColumn, State(currentLevel, right) :: rest) match {
          case Layout.Newline(level,_) => render(
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

  /** Renders the given `LayoutSpec`, rendering soft breaks as newlines whenever
    * the length of the line exceeds the given `width`.
    */
  def apply(target: LayoutSpec, width: Int): Layout = render(width,0,List(State(0,target)))

  /** Renders the given `LayoutSpec`, rendering soft breaks as newlines whenever
    * the length of the line exceeds 80 characters.
    */
  def apply(target: LayoutSpec): Layout = render(80,0,List(State(0,target)))

}
