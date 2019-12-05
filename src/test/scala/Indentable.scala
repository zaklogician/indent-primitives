package indentor.test;

import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Gen
import org.scalacheck.Gen.{oneOf, listOf, alphaStr, numChar}

import indentor.TestObject;

object TestTestObject extends Properties("TestObject") {
  
  
  //////////////////////////////////////////////////////////////////////////////
  // GENERATORS (for randomized testing):
  
  val testObject = Gen.oneOf(TestObject,TestObject)
  val individual = Gen.listOf(testObject).suchThat(_.length > 1)
  
  //////////////////////////////////////////////////////////////////////////////
  // TESTS:

  property("name reflexive") = Prop.forAll(testObject) { i =>
    i.name == i.name
  }
  
  property("name as expected") = Prop {
    TestObject.name == "TestObject"
  }
  
}
