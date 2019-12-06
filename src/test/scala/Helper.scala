package indentor.test;

import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

import indentor.LayoutSpec;
import indentor.Layout;
import indentor.Render;

object Helper {

  //////////////////////////////////////////////////////////////////////////////
  // HELPER FUNCTIONS:

  /* Adds the given prefix to all literals. Adds further prefixes "l" and "r" under all binary
   * operations, thus ensuring that all literals in the given string occur exactly once.
   */
  def addPrefix(i: LayoutSpec, p: String): LayoutSpec = i match {
    case LayoutSpec.Literal(text) =>
      LayoutSpec.Literal(p + text)
    case LayoutSpec.AddLevel(level, body) =>
      LayoutSpec.AddLevel(level, addPrefix(body, p))
    case LayoutSpec.SetLevel(level, body) =>
      LayoutSpec.SetLevel(level, addPrefix(body, p))
    case LayoutSpec.Concat(left, right) =>
      LayoutSpec.Concat(addPrefix(left, p + "l"), addPrefix(right, p + "r"))
    case LayoutSpec.WithCurrentColumn(body) =>
      LayoutSpec.WithCurrentColumn { c => addPrefix(body(c),p) }
    case LayoutSpec.WithCurrentLevel(body) =>
      LayoutSpec.WithCurrentLevel { l => addPrefix(body(l),p) }
    case LayoutSpec.StrainToRightBegin(left, right) =>
      LayoutSpec.StrainToRightBegin( addPrefix(left,p + "l"), addPrefix(right,p + "r") )
    case _ => i
  }

  /* Ensures that all literals in the given LayoutSpec are unique up to equality. */
  def uniqueLiterals(i: LayoutSpec): LayoutSpec = addPrefix(i,"")

  case class LiteralPos(line: Int, column: Int, literal: String) {
    def toRightOf(that: LiteralPos): Boolean =
      this.column >= that.column

    def toLeftOf(that: LiteralPos): Boolean =
      this.column <= that.column

    def below(that: LiteralPos): Boolean = this.line > that.line

    def toRightOrBelow(that:LiteralPos): Boolean =
      (this toRightOf that) || (this below that)

    // positive precisely if this is strictly to the right of that
    def columnDifference(that: LiteralPos): Int = this.column - that.column
  }

  // returns the given `LayoutSpec` with all non-forward positioning blocks removed
  // these are AddLevels for negative level, SetLevels and Strains
  def removeNonforward(i: LayoutSpec): LayoutSpec = i match {
    case LayoutSpec.AddLevel(level, body) =>
      if (level >= 0) LayoutSpec.AddLevel(level, removeNonforward(body)) else LayoutSpec.Empty
    case LayoutSpec.SetLevel(level, body) => LayoutSpec.Empty
    case LayoutSpec.Concat(left, right) =>
      LayoutSpec.Concat(removeNonforward(left), removeNonforward(right))
    case LayoutSpec.WithCurrentColumn(body) =>
      LayoutSpec.WithCurrentColumn { c => removeNonforward(body(c)) }
    case LayoutSpec.WithCurrentLevel(body) =>
      LayoutSpec.WithCurrentLevel { l => removeNonforward(body(l)) }
    case LayoutSpec.StrainToRightBegin(left, right) => LayoutSpec.Empty
    case _ => i
  }
  
  // returns a list of all literals in the given Layout, along with all their positions,
  // in the order they occur in the structure.
  def getLiterals(i: Layout, currentLine: Int, currentColumn: Int): List[LiteralPos] = i match {
    case Layout.Empty => Nil
    case Layout.Literal(head, tail) =>
      LiteralPos(currentLine,currentColumn, head) ::
      getLiterals(tail, currentLine, currentColumn + head.length)
    case Layout.Newline(level, body) =>
      getLiterals(body, currentLine + 1, level)
  }

  def render(i: LayoutSpec): Layout = Render(i,80)
  def literals(i: LayoutSpec): List[LiteralPos] = getLiterals( render(i), 0, 0 )
  
  def firstOfFirstLine(i: LayoutSpec): Option[LiteralPos] = 
    literals(i) match {
      case Nil  => None
      case lits => lits.groupBy(_.line).minBy(_._1)._2.headOption
    }
    
  def lastOfFirstLine(i: LayoutSpec): Option[LiteralPos] = 
    literals(i) match {
      case Nil  => None
      case lits => lits.groupBy(_.line).minBy(_._1)._2.lastOption
    }
    
  def firstOfLastLine(i: LayoutSpec): Option[LiteralPos] = 
    literals(i) match {
      case Nil  => None
      case lits => lits.groupBy(_.line).maxBy(_._1)._2.headOption
    }
    
  def lastOfLastLine(i: LayoutSpec): Option[LiteralPos] = 
    literals(i) match {
      case Nil  => None
      case lits => lits.groupBy(_.line).maxBy(_._1)._2.lastOption
    }

}
