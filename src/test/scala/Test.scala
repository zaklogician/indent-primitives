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

object TestIndentable extends Properties("Indentable") {

  // CONFIG //
  
  /* If `fullUnicode` is set to true, we allow arbitrary Unicode sequences in our tests.
   * These can be extremely weird, containing direction modifiers, etc. which provide
   * spurious counterexamples to the semantics.
   */
  val fullUnicode: Boolean = false

  val allowedChars: String =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" +
    "0123456789:;<=>?@[\\]^_`\"' \t\n" +
    "∀∁∂∃∄∅∆∇∈∉∊∋∌∍∎∏∐∑−∓∔∕∖∗∘∙√∛∜∝∞∟" +
    "≠≡≢≣≤≥≦≧≨≩≪≫≬≭≮≯≰≱≲≳≴≵≶≷≸≹≺≻≼≽≾≿" +
    "⊀⊁⊂⊃⊄⊅⊆⊇⊈⊉⊊⊋⊌⊍⊎⊏⊐⊑⊒⊓⊔⊕⊖⊗⊘⊙⊚⊛⊜⊝⊞⊟" +
    "⊠⊡⊢⊣⊤⊥⊦⊧⊨⊩⊪⊫⊬⊭⊮⊯⊰⊱⊲⊳⊴⊵⊶⊷⊸⊹⊺⊻⊼⊽⊾⊿" +
    "⋀⋁⋂⋃⋄⋅⋆⋇⋈⋉⋊⋋⋌⋍⋎⋏⋐⋑⋒⋓⋔⋕⋖⋗⋘⋙⋚⋛⋜⋝⋞⋟"

  // HELPER FUNCTIONS //

  /* Adds prefixes to all literals, making sure that they are all unique. */
  def addPrefix(i: Indentable, p: String): Indentable = i match {
    case Indentable.Literal(text) =>
      Indentable.Literal(p + text)
    case Indentable.AddLevel(level, body) =>
      Indentable.AddLevel(level, addPrefix(body,p))
    case Indentable.SetLevel(level, body) =>
      Indentable.SetLevel(level, addPrefix(body,p))
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

  case class LiteralStart(line: Int, column: Int, literal: String) {
    def toRightOf(that: LiteralStart): Boolean =
      this.column >= that.column

    def toLeftOf(that: LiteralStart): Boolean =
      this.column <= that.column

    def below(that: LiteralStart): Boolean = this.line > that.line

    def toRightOrBelow(that:LiteralStart): Boolean =
      (this toRightOf that) || (this below that)
  }

  def getPositions(i: Indented, currentLine: Int, currentColumn: Int): List[LiteralStart] = i match {
    case Indented.Empty => Nil
    case Indented.Literal(head, tail) =>
      LiteralStart(currentLine,currentColumn, head) ::
      getPositions(tail, currentLine, currentColumn + head.length)
    case Indented.Newline(level, body) =>
      getPositions(body, currentLine + 1, level)
  }

  def positions(i: Indented): List[LiteralStart] = getPositions(i,0,0)

  def literals(i: Indented): List[String] = positions(i).map(_.literal)

  def render(i: Indentable): Indented = Render(i)
  
  // GENERATORS //

  val rawBase: Gen[Indentable] =
    Gen.oneOf( Indentable.Empty, Indentable.SoftBreak, Indentable.HardBreak)

  val genSafeLine: Gen[String] = for {
    str <- Gen.listOf(Gen.oneOf(allowedChars))
  } yield str.mkString("").take(80)

  val rawLiteral: Gen[Indentable] = for {
    str <- if (fullUnicode) arbitrary[String] else genSafeLine
  } yield Indentable.Text(str take 80)

  def rawLevel(depth: Int): Gen[Indentable] = for {
    level <- Gen.choose(-5,5)
    body <- rawIndentable(depth-1)
    ctor <- Gen.oneOf(Indentable.AddLevel,Indentable.SetLevel)
  } yield ctor(level,body)

  def rawConcat(depth: Int): Gen[Indentable] = for {
    left <- rawIndentable(depth-1)
    right <- rawIndentable(depth-1)
  } yield (left ++ right)

  def rawLeft(depth: Int): Gen[Indentable] = for {
    left <- rawIndentable(depth-1)
    right <- rawIndentable(depth-1)
  } yield (left << right)

  def rawRight(depth: Int): Gen[Indentable] = for {
    left <- rawIndentable(depth-1)
    right <- rawIndentable(depth-1)
  } yield (left >> right)

  def rawIndentable(depth: Int): Gen[Indentable] =
    if (depth < 0) Gen.oneOf(rawBase, rawLiteral)
    else Gen.oneOf( rawBase, rawLiteral
                  , rawLevel(depth), rawConcat(depth), rawLeft(depth), rawRight(depth)
                  )
  
  def genIndentable: Gen[Indentable] = for {
    i <- rawIndentable(6)
  } yield addPrefix(i,"")
  implicit val arbIndentable: Arbitrary[Indentable] = Arbitrary(genIndentable)

  //////////////////////////////////////////////////////////////////////////////
  // TESTS:

  property("Text removes all newlines") = Prop.forAll { (i: String) =>
    literals(render(Indentable.Text(i))) forall { l => !l.contains("\n") }
  }

  property("Text renders as itself") = Prop.forAll { (i: String) =>
    render( Indentable.Text(i) ).asString == i
  }
  
  property("Empty renders as the empty string") = Prop {
    render( Indentable.Empty ).asString == ""
  }

  property("++ satisfies semantics") = Prop.forAll { (l: Indentable, r: Indentable) =>
    // we use unique prefixes to indentify which `Indentable` literals come from
    val ll = addPrefix(l,"L")
    val rr = addPrefix(r,"R")
    val kk = ll ++ rr
    val lpos = positions( render(kk) ).filter(x => x.literal.startsWith("L"))
    val rpos = positions( render(kk) ).filter(x => x.literal.startsWith("R"))
    rpos forall { r => lpos forall {l => r toRightOrBelow l} }
  }

  property("++ associative") = Prop.forAll { (a: Indentable, b: Indentable, c: Indentable) =>
    render(a ++ (b ++ c)).toString == render((a ++ b) ++ c).toString
  }
  
  property(">> satisfies semantics") = Prop.forAll { (l: Indentable, r: Indentable) =>
    // we use unique prefixes to indentify which `Indentable` literals come from
    val ll = addPrefix(l,"L")
    val rr = addPrefix(r,"R")
    val kk = ll >> rr
    val lpos = positions( render(kk) ).filter(x => x.literal.startsWith("L")).lastOption
    val rpos = positions( render(kk) ).filter(x => x.literal.startsWith("R"))
    val result = lpos match {
      case None => true
      case Some(l) => rpos forall { r => r toRightOf l }
    }
    result
  }
  
  property("<< satisfies semantics") = Prop.forAll { (l: Indentable, r: Indentable) =>
    // we use unique prefixes to indentify which `Indentable` literals come from
    val ll = addPrefix(l,"L")
    val rr = addPrefix(r,"R")
    val kk = ll << rr
    val lpos = positions( render(kk) ).filter(x => x.literal.startsWith("L"))
    val rpos = positions( render(kk) ).filter(x => x.literal.startsWith("R")).headOption
    val result = rpos match {
      case None => true
      case Some(r) => lpos forall { l => l toLeftOf r }
    }
    result
  }

  property("equality reflexive") = Prop.forAll { (i: Indentable) =>
    i == i
  }


}
