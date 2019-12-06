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

import indentor.test.Helper._;

object TestLayoutSpec extends Properties("LayoutSpec") {

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

  val rawBase: Gen[LayoutSpec] =
    Gen.oneOf( LayoutSpec.Empty, LayoutSpec.SoftBreak, LayoutSpec.HardBreak)

  val genSafeLine: Gen[String] = for {
    str <- Gen.listOf(Gen.oneOf(allowedChars))
  } yield str.mkString("")

  val rawLiteral: Gen[LayoutSpec] = for {
    str <- if (fullUnicode) arbitrary[String] else genSafeLine
  } yield LayoutSpec.Text(str take 80)

  def rawLevel(depth: Int): Gen[LayoutSpec] = for {
    level <- Gen.choose(-5,5)
    body <- rawLayoutSpec(depth-1)
    ctor <- Gen.oneOf(LayoutSpec.AddLevel,LayoutSpec.SetLevel)
  } yield ctor(level,body)

  def rawConcat(depth: Int): Gen[LayoutSpec] = for {
    left <- rawLayoutSpec(depth-1)
    right <- rawLayoutSpec(depth-1)
  } yield (left ++ right)

  def rawLeft(depth: Int): Gen[LayoutSpec] = for {
    left <- rawLayoutSpec(depth-1)
    right <- rawLayoutSpec(depth-1)
  } yield (left << right)

  def rawRight(depth: Int): Gen[LayoutSpec] = for {
    left <- rawLayoutSpec(depth-1)
    right <- rawLayoutSpec(depth-1)
  } yield (left >> right)

  def rawLayoutSpec(depth: Int): Gen[LayoutSpec] =
    if (depth < 0) Gen.oneOf(rawBase, rawLiteral)
    else Gen.oneOf( rawBase, rawLiteral
                  , rawLevel(depth), rawConcat(depth), rawLeft(depth), rawRight(depth)
                  )
  
  def genLayoutSpec: Gen[LayoutSpec] = for {
    i <- rawLayoutSpec(8)
  } yield uniqueLiterals(i)
  implicit val arbLayoutSpec: Arbitrary[LayoutSpec] = Arbitrary(genLayoutSpec)

  //////////////////////////////////////////////////////////////////////////////
  // PROPERTIES:

  property("Text removes all newlines") = Prop.forAll { (i: String) =>
    literals( LayoutSpec.Text(i) ) forall { l => !l.literal.contains("\n") }
  }

  property("Text renders as itself") = Prop.forAll { (i: String) =>
    render( LayoutSpec.Text(i) ).asString == i
  }
  
  property("Empty renders as the empty string") = Prop {
    render( LayoutSpec.Empty ).asString == ""
  }

  property("++ satisfies semantics") = Prop.forAll { (left: LayoutSpec, right: LayoutSpec) =>
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

  property("++ associative") = Prop.forAll { (a: LayoutSpec, b: LayoutSpec, c: LayoutSpec) =>
    render(a ++ (b ++ c)).toString == render((a ++ b) ++ c).toString
  }
  
  property(">> satisfies forward semantics") = Prop.forAll { (left: LayoutSpec, right: LayoutSpec) =>
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

  property("<< satisfies forward semantics") = Prop.forAll { (left: LayoutSpec, right: LayoutSpec) =>
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

  property("equality reflexive") = Prop.forAll { (i: LayoutSpec) =>
    i == i
  }

}
