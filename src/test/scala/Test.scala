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

import indentor.test.Helper._;

object TestIndentable extends Properties("Indentable") {

  //////////////////////////////////////////////////////////////////////////////
  // TEST CONFIGURATION:
  
  /* If `fullUnicode` is set to true, we allow arbitrary Unicode sequences in our tests.
   * These can be extremely weird, containing direction modifiers, etc. which may provide
   * spurious counterexamples to the semantics.
   */
  val fullUnicode: Boolean = false

  /* If `fullUnicode` is set to false, we only allow the Unicode codepoints contained in
   * the string `allowedChars`. These cover many basic mathematical symbols, along with
   * many common ASCII characters.
   */
  val allowedChars: String =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" +
    "0123456789:;<=>?@[\\]^_`\"' ()\n" +
    "∀∁∂∃∄∅∆∇∈∉∊∋∌∍∎∏∐∑−∓∔∕∖∗∘∙√∛∜∝∞∟" +
    "≠≡≢≣≤≥≦≧≨≩≪≫≬≭≮≯≰≱≲≳≴≵≶≷≸≹≺≻≼≽≾≿" +
    "⊀⊁⊂⊃⊄⊅⊆⊇⊈⊉⊊⊋⊌⊍⊎⊏⊐⊑⊒⊓⊔⊕⊖⊗⊘⊙⊚⊛⊜⊝⊞⊟" +
    "⊠⊡⊢⊣⊤⊥⊦⊧⊨⊩⊪⊫⊬⊭⊮⊯⊰⊱⊲⊳⊴⊵⊶⊷⊸⊹⊺⊻⊼⊽⊾⊿" +
    "⋀⋁⋂⋃⋄⋅⋆⋇⋈⋉⋊⋋⋌⋍⋎⋏⋐⋑⋒⋓⋔⋕⋖⋗⋘⋙⋚⋛⋜⋝⋞⋟"

  //////////////////////////////////////////////////////////////////////////////
  // GENERATORS:

  val rawBase: Gen[Indentable] =
    Gen.oneOf( Indentable.Empty, Indentable.SoftBreak, Indentable.HardBreak)

  val genSafeLine: Gen[String] = for {
    str <- Gen.listOf(Gen.oneOf(allowedChars))
  } yield str.mkString("")

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
    i <- rawIndentable(8)
  } yield uniqueLiterals(i)
  implicit val arbIndentable: Arbitrary[Indentable] = Arbitrary(genIndentable)

  //////////////////////////////////////////////////////////////////////////////
  // PROPERTIES:

  property("Text removes all newlines") = Prop.forAll { (i: String) =>
    literals( Indentable.Text(i) ) forall { l => !l.literal.contains("\n") }
  }

  property("Text renders as itself") = Prop.forAll { (i: String) =>
    render( Indentable.Text(i) ).asString == i
  }
  
  property("Empty renders as the empty string") = Prop {
    render( Indentable.Empty ).asString == ""
  }

  property("++ satisfies semantics") = Prop.forAll { (left: Indentable, right: Indentable) =>
    // we ensure unique prefixes
    val ll = addPrefix(left,"L")
    val rr = addPrefix(right,"R")
    val leftLiterals = literals(ll).map(_.literal)
    val rightLiterals = literals(rr).map(_.literal)
    val result = ll ++ rr
    val resultLeftLiterals = literals(result).filter(x => leftLiterals contains x.literal)
    val resultRightLiterals = literals(result).filter(x => rightLiterals contains x.literal)
    resultRightLiterals forall { r =>
      resultLeftLiterals.forall(l => r toRightOrBelow l)
    }
  }

  property("++ associative") = Prop.forAll { (a: Indentable, b: Indentable, c: Indentable) =>
    render(a ++ (b ++ c)).toString == render((a ++ b) ++ c).toString
  }
  
  property(">> satisfies forward semantics") = Prop.forAll { (left: Indentable, right: Indentable) =>
    // we ensure unique prefixes
    val ll = addPrefix( removeNonforward(left),"L")
    val rr = addPrefix( removeNonforward(right), "R")
    val leftLiterals = literals(ll).map(_.literal)
    val rightLiterals = literals(rr).map(_.literal)
    val result = ll >> rr
    val resultLeftLiterals = literals(result).filter(x => leftLiterals contains x.literal)
    val resultRightLiterals = literals(result).filter(x => rightLiterals contains x.literal)
    lastOfLastLine(ll) match {
      case None => true
      case Some(l) => resultRightLiterals forall { r => r toRightOf l }
    }
  }

  property("<< satisfies forward semantics") = Prop.forAll { (left: Indentable, right: Indentable) =>
    // we ensure unique prefixes
    val ll = addPrefix( removeNonforward(left),"L")
    val rr = addPrefix( removeNonforward(right), "R")
    val leftLiterals = literals(ll).map(_.literal)
    val rightLiterals = literals(rr).map(_.literal)
    val result = ll << rr
    val resultLeftLiterals = literals(result).filter(x => leftLiterals contains x.literal)
    val resultRightLiterals = literals(result).filter(x => rightLiterals contains x.literal)
    (for {
      l <- firstOfLastLine(ll)
      r <- firstOfFirstLine(rr)
    } yield (l toLeftOf r)) getOrElse true
  }

  property("equality reflexive") = Prop.forAll { (i: Indentable) =>
    i == i
  }

}
