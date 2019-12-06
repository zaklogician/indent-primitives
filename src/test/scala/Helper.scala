package indentor.test;

import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

import indentor.Indentable;
import indentor.Indented;
import indentor.Render;

object Helper {

  //////////////////////////////////////////////////////////////////////////////
  // HELPER FUNCTIONS:

  /* Adds the given prefix to all literals. Adds further prefixes "l" and "r" under all binary
   * operations, thus ensuring that all literals in the given string occur exactly once.
   */
  def addPrefix(i: Indentable, p: String): Indentable = i match {
    case Indentable.Literal(text) =>
      Indentable.Literal(p + text)
    case Indentable.AddLevel(level, body) =>
      Indentable.AddLevel(level, addPrefix(body, p))
    case Indentable.SetLevel(level, body) =>
      Indentable.SetLevel(level, addPrefix(body, p))
    case Indentable.Concat(left, right) =>
      Indentable.Concat(addPrefix(left, p + "l"), addPrefix(right, p + "r"))
    case Indentable.WithCurrentColumn(body) =>
      Indentable.WithCurrentColumn { c => addPrefix(body(c),p) }
    case Indentable.WithCurrentLevel(body) =>
      Indentable.WithCurrentLevel { l => addPrefix(body(l),p) }
    case Indentable.StrainToRightBegin(left, right) =>
      Indentable.StrainToRightBegin( addPrefix(left,p + "l"), addPrefix(right,p + "r") )
    case _ => i
  }

  /* Ensures that all literals in the given Indentable are unique up to equality. */
  def uniqueLiterals(i: Indentable): Indentable = addPrefix(i,"")

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

  // returns the given `Indentable` with all non-forward positioning blocks removed
  // these are AddLevels for negative level, SetLevels and Strains
  def removeNonforward(i: Indentable): Indentable = i match {
    case Indentable.AddLevel(level, body) =>
      if (level >= 0) Indentable.AddLevel(level, removeNonforward(body)) else Indentable.Empty
    case Indentable.SetLevel(level, body) => Indentable.Empty
    case Indentable.Concat(left, right) =>
      Indentable.Concat(removeNonforward(left), removeNonforward(right))
    case Indentable.WithCurrentColumn(body) =>
      Indentable.WithCurrentColumn { c => removeNonforward(body(c)) }
    case Indentable.WithCurrentLevel(body) =>
      Indentable.WithCurrentLevel { l => removeNonforward(body(l)) }
    case Indentable.StrainToRightBegin(left, right) => Indentable.Empty
    case _ => i
  }
  
  // returns a list of all literals in the given Indented, along with all their positions,
  // in the order they occur in the structure.
  def getLiterals(i: Indented, currentLine: Int, currentColumn: Int): List[LiteralPos] = i match {
    case Indented.Empty => Nil
    case Indented.Literal(head, tail) =>
      LiteralPos(currentLine,currentColumn, head) ::
      getLiterals(tail, currentLine, currentColumn + head.length)
    case Indented.Newline(level, body) =>
      getLiterals(body, currentLine + 1, level)
  }

  def render(i: Indentable): Indented = Render(i,80)
  def literals(i: Indentable): List[LiteralPos] = getLiterals( render(i), 0, 0 )
  
  def firstOfFirstLine(i: Indentable): Option[LiteralPos] = 
    literals(i) match {
      case Nil  => None
      case lits => lits.groupBy(_.line).minBy(_._1)._2.headOption
    }
    
  def lastOfFirstLine(i: Indentable): Option[LiteralPos] = 
    literals(i) match {
      case Nil  => None
      case lits => lits.groupBy(_.line).minBy(_._1)._2.lastOption
    }
    
  def firstOfLastLine(i: Indentable): Option[LiteralPos] = 
    literals(i) match {
      case Nil  => None
      case lits => lits.groupBy(_.line).maxBy(_._1)._2.headOption
    }
    
  def lastOfLastLine(i: Indentable): Option[LiteralPos] = 
    literals(i) match {
      case Nil  => None
      case lits => lits.groupBy(_.line).maxBy(_._1)._2.lastOption
    }

}
