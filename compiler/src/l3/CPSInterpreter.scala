package l3

import scala.collection.mutable.{ Map => MutableMap }
import IO._

/**
 * A tree-based interpreter for the CPS languages.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

abstract class CPSInterpreter[M <: CPSTreeModule](protected val treeModule: M) {
  import treeModule._

  def apply(tree: Tree): Unit =
    eval(tree, emptyEnv)

  def statistics(tree: Tree): Unit =
    ()

  protected sealed trait Value
  protected case class FunV(retC: Name, args: Seq[Name], body: Tree, env: Env)
      extends Value
  protected case class CntV(args: Seq[Name], body: Tree, env: Env)
      extends Value

  protected type Env = PartialFunction[Name, Value]
  protected val emptyEnv: Env = Map.empty

  private def eval(tree: Tree, env: Env): Unit = {
    statistics(tree: Tree)
    (tree: @unchecked) match {
      case LetL(name, lit, body) =>
        eval(body, Map(name -> evalLit(lit)) orElse env)

      case LetP(name, prim, args, body) =>
        eval(body, Map(name -> evalValuePrim(prim, args map env)) orElse env)

      case LetC(cnts, body) =>
        val recEnv = MutableMap[Name, Value]()
        val env1 = recEnv orElse env
        for (CntDef(name, args, body) <- cnts)
          recEnv(name) = CntV(args, body, env1)
        eval(body, env1)

      case LetF(funs, body) =>
        val recEnv = MutableMap[Name, Value]()
        val env1 = recEnv orElse env
        for (FunDef(name, retC, args, body) <- funs)
          recEnv(name) = wrapFunV(FunV(retC, args, body, env1))
        eval(body, env1)

      case AppC(cnt, args) =>
        val CntV(cArgs, cBody, cEnv) = env(cnt)
        assume(cArgs.length == args.length)
        eval(cBody, Map(cArgs zip (args map env) : _*) orElse cEnv)

      case AppF(fun, retC, args) =>
        val FunV(fRetC, fArgs, fBody, fEnv) = unwrapFunV(env(fun))
        assume(fArgs.length == args.length)
        eval(fBody,
             Map((fRetC +: fArgs) zip ((retC +: args) map env) : _*) orElse fEnv)

      case If(cond, args, thenC, elseC) =>
        val cnt = if (evalTestPrim(cond, args map env)) thenC else elseC
        val cntV = env(cnt).asInstanceOf[CntV]
        eval(cntV.body, cntV.env)

      case Halt =>
        ()
    }
  }

  protected def wrapFunV(funV: FunV): Value
  protected def unwrapFunV(v: Value): FunV

  protected def evalLit(l: Literal): Value
  protected def evalValuePrim(p: ValuePrimitive, args: Seq[Value]): Value
  protected def evalTestPrim(p: TestPrimitive, args: Seq[Value]): Boolean
}

object CPSInterpreterHigh extends CPSInterpreterHigh
class CPSInterpreterHigh extends CPSInterpreter(SymbolicCPSTreeModule)
    with (SymbolicCPSTreeModule.Tree => Unit) {
  import treeModule._

  private case class BlockV(tag: Int, contents: Array[Value]) extends Value
  private case class IntV(value: Int) extends Value
  private case class CharV(value: Char) extends Value
  private case class BooleanV(value: Boolean) extends Value
  private case object UnitV extends Value

  protected def wrapFunV(funV: FunV): Value =
    BlockV(BlockTag.Function.id, Array(funV))
  protected def unwrapFunV(v: Value): FunV = v match {
    case BlockV(_, Array(funV: FunV)) => funV
  }

  protected def evalLit(l: Literal): Value = l match {
    case IntLit(i) => IntV(i)
    case CharLit(c) => CharV(c)
    case BooleanLit(b) => BooleanV(b)
    case UnitLit => UnitV
  }

  protected def evalValuePrim(p: ValuePrimitive, args: Seq[Value]): Value =
    (p, args) match {
      case (L3BlockAlloc(t), Seq(IntV(i))) => BlockV(t, Array.fill(i)(UnitV))
      case (L3BlockTag, Seq(BlockV(t, _))) => IntV(t)
      case (L3BlockLength, Seq(BlockV(_, c))) => IntV(c.length)
      case (L3BlockGet, Seq(BlockV(_, v), IntV(i))) => v(i)
      case (L3BlockSet, Seq(BlockV(_, v), IntV(i), o)) => v(i) = o; UnitV

      case (L3IntAdd, Seq(IntV(v1), IntV(v2))) => IntV(v1 + v2)
      case (L3IntSub, Seq(IntV(v1), IntV(v2))) => IntV(v1 - v2)
      case (L3IntMul, Seq(IntV(v1), IntV(v2))) => IntV(v1 * v2)
      case (L3IntDiv, Seq(IntV(v1), IntV(v2))) => IntV(v1 / v2)
      case (L3IntMod, Seq(IntV(v1), IntV(v2))) => IntV(v1 % v2)
      case (L3IntToChar, Seq(IntV(v))) => CharV(v.toChar)

      case (L3IntArithShiftLeft, Seq(IntV(v1), IntV(v2))) => IntV(v1 << v2)
      case (L3IntArithShiftRight, Seq(IntV(v1), IntV(v2))) => IntV(v1 >> v2)
      case (L3IntBitwiseAnd, Seq(IntV(v1), IntV(v2))) => IntV(v1 & v2)
      case (L3IntBitwiseOr, Seq(IntV(v1), IntV(v2))) => IntV(v1 | v2)
      case (L3IntBitwiseXOr, Seq(IntV(v1), IntV(v2))) => IntV(v1 ^ v2)

      case (L3CharRead, Seq()) => CharV(readChar())
      case (L3CharPrint, Seq(CharV(c))) => printChar(c); UnitV
      case (L3CharToInt, Seq(CharV(c))) => IntV(c.toInt)
    }

  protected def evalTestPrim(p: TestPrimitive, args: Seq[Value]): Boolean =
    (p, args) match {
      case (L3BlockP, Seq(BlockV(_, _))) => true
      case (L3BlockP, Seq(_)) => false

      case (L3IntP, Seq(IntV(_))) => true
      case (L3IntP, Seq(_)) => false
      case (L3IntLt, Seq(IntV(v1), IntV(v2))) => v1 < v2
      case (L3IntLe, Seq(IntV(v1), IntV(v2))) => v1 <= v2
      case (L3IntGe, Seq(IntV(v1), IntV(v2))) => v1 >= v2
      case (L3IntGt, Seq(IntV(v1), IntV(v2))) => v1 > v2

      case (L3CharP, Seq(CharV(_))) => true
      case (L3CharP, Seq(_)) => false

      case (L3BoolP, Seq(BooleanV(_))) => true
      case (L3BoolP, Seq(_)) => false

      case (L3UnitP, Seq(UnitV)) => true
      case (L3UnitP, Seq(_)) => false

      case (L3Eq, Seq(v1, v2)) => v1 == v2
      case (L3Ne, Seq(v1, v2)) => v1 != v2
    }
}
