package indentor;

import indentor.LayoutSpec._

object Main {

  /* Our example represents the following test script:
   * ```
   * apply (simp add: conjI disjE impI)
   * apply (induction n)
   *  (* comment aligns with next line *)
   *  apply simp
   * (* comment aligns with next line *)
     find_theorems blah
   *  apply arith
   * apply simp
   * ```
   */

  val command1: LayoutSpec = // Isabelle commands start with a hard break
    HardBreak ++ (
      Literal("apply") >>| (
        Literal("(") ++ (
          Literal("simp") >>|| (
            Literal("add:") >>|| (
              Literal("conjI") ++|| (
                Literal("disjI") ++|| (
                  Literal("impI") ++ (
                    Literal(")")
                  )
                )
              )
            )
          )
        )
      )
    )

  val command2: LayoutSpec =
    HardBreak ++ (
      Literal("apply") >>| (
        Literal("(") ++ (
          Literal("induction") >>|| (
            Literal("n") ++ (
              Literal(")")
            )
          )
        )
      )
    )

  val comment1: LayoutSpec =
    HardBreak ++ (
      Literal("(* comment aligns with next line *)")
    )

  val command3: LayoutSpec =
    HardBreak ++ (
      Literal("apply") >>| (
        Literal("simp")
      )
    )

  val lookup1: LayoutSpec =
    SetLevel(0,
      HardBreak ++ (
        Literal("find_theorems") >>| (
          Literal("blah")
        )
      )
    )

  val command4: LayoutSpec =
    HardBreak ++ (
      Literal("apply") >>| (
        Literal("arith")
      )
    )

  val example: LayoutSpec =
    command1 ++ AddLevel(0,
      command2 ++ AddLevel(1,
        comment1 << (
          command3 ++ AddLevel(0, 
            comment1 << (
              lookup1 ++ (
                command4 ++ AddLevel(-1, 
                  command3
                )
              )
            )
          )
        )
      )
    )

  def printExample(columns: Int): Unit = {
    println("\n" + columns + " columns")
    println( List.fill(columns)('-').mkString )
    println( Render(example,columns-3).asString )
  }

  def main(args: Array[String]): Unit = {
    println("If you're reading this, the build worked!")
    printExample(10)
    printExample(20)
    printExample(30)
    printExample(40)
  }
  
}
