package l3

import scala.util.parsing.input.Positional

/**
 * A module for CL₃ trees.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

trait CL3TreeModule {
  type Name
  type Primitive

  sealed trait Tree extends Positional
  case class Let(bindings: List[(Name, Tree)], body: Tree) extends Tree
  case class LetRec(functions: List[FunDef], body: Tree) extends Tree
  case class If(cond: Tree, thenE: Tree, elseE: Tree) extends Tree
  case class App(fun: Tree, args: List[Tree]) extends Tree
  case class Prim(prim: Primitive, args: List[Tree]) extends Tree
  case class Ident(name: Name) extends Tree
  case class Lit(value: CL3Literal) extends Tree

  case class FunDef(name: Name, args: List[Name], body: Tree)
      extends Positional
}

/**
 * Module for trees after parsing: names and primitives are
 * represented as strings.
 */
object NominalCL3TreeModule extends CL3TreeModule {
  type Name = String
  type Primitive = String
}

/**
 * Module for trees after name analysis: names are represented as
 * symbols (globally-unique names) and primitives as objects.
 */
object SymbolicCL3TreeModule extends CL3TreeModule {
  type Name = Symbol
  type Primitive = L3Primitive
}
