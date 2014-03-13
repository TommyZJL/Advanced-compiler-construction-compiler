package l3

/**
 * A module for CPS trees.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

trait CPSTreeModule {
  type Name
  type ValuePrimitive
  type TestPrimitive
  type Literal

  sealed trait Tree {
    /**
     * Produce a new version of the tree where all names have been
     * substituted according to the given partial function. Names
     * that are not in the partial function's domain are left
     * untouched.
     */
    def subst(partialS: PartialFunction[Name, Name]): Tree = {
      val s = { n: Name => if (partialS isDefinedAt n) partialS(n) else n }

      def substIn(t: Tree): Tree = t match {
        case LetL(name, value, body) =>
          LetL(s(name), value, substIn(body))
        case LetP(name, prim, args, body) =>
          LetP(s(name), prim, args map s, substIn(body))
        case LetC(continuations, body) =>
          val substContinuations = continuations map {
            case CntDef(name, args, body) =>
              CntDef(s(name), args map s, substIn(body))
          }
          LetC(substContinuations, substIn(body))
        case LetF(functions, body) =>
          val substFunctions = functions map {
            case FunDef(name, retC, args, body) =>
              FunDef(s(name), s(retC), args map s, substIn(body))
          }
          LetF(substFunctions, substIn(body))
        case AppC(cont, args) =>
          AppC(s(cont), args map s)
        case AppF(fun, retC, args) =>
          AppF(s(fun), s(retC), args map s)
        case If(cond, args, thenC, elseC) =>
          If(cond, args map s, s(thenC), s(elseC))
        case Halt =>
          Halt
      }

      substIn(this)
    }
  }

  case class LetL(name: Name, value: Literal, body: Tree) extends Tree
  case class LetP(name: Name, prim: ValuePrimitive, args: List[Name], body:Tree)
       extends Tree
  case class LetC(continuations: List[CntDef], body: Tree) extends Tree
  case class LetF(functions: List[FunDef], body: Tree) extends Tree
  case class AppC(cont: Name, args: List[Name]) extends Tree
  case class AppF(fun: Name, retC: Name, args: List[Name]) extends Tree
  case class If(cond: TestPrimitive, args: List[Name], thenC: Name, elseC: Name)
       extends Tree
  case object Halt extends Tree

  case class CntDef(name: Name, args: List[Name], body: Tree)
  case class FunDef(name: Name, retC: Name, args: List[Name], body: Tree)
}

/**
 * Module for "high-level" CPS trees: the full L3 literals and
 * primitives are available.
 */
object SymbolicCPSTreeModule extends CPSTreeModule {
  type Name = Symbol
  type ValuePrimitive = L3ValuePrimitive
  type TestPrimitive = L3TestPrimitive
  type Literal = CL3Literal
}

/**
 * Module for "low-level" CPS trees: the only literal values are
 * integers, and the primitives work on integers and/or pointers to
 * heap-allocated blocks.
 */
object SymbolicCPSTreeModuleLow extends CPSTreeModule {
  type Name = Symbol
  type ValuePrimitive = CPSValuePrimitive
  type TestPrimitive = CPSTestPrimitive
  type Literal = Int
}
