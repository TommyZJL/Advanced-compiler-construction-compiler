package l3

import l3.{ SymbolicCL3TreeModule => S }
import l3.{ SymbolicCPSTreeModule => C }

object CL3ToCPSTranslator extends (S.Tree => C.Tree) {
  def apply(tree: S.Tree): C.Tree = {
    nonTail(tree, _ => C.Halt)
  }

  private def nonTail(tree: S.Tree, ctx: Symbol => C.Tree): C.Tree =
    tree match {
      case S.Let((name, value) :: rest, body) =>
        tempLetC("lc", List(name), nonTail(S.Let(rest, body), ctx)) { lc =>
          tail(value, lc) }

      case S.Let(List(), body) =>
        nonTail(body, ctx)

      // TODO

    }

  private def nonTail_*(trees: List[S.Tree], ctx: List[Symbol] => C.Tree)
      : C.Tree =
    trees match {
      case List() =>
        ctx(List())
      case t :: ts =>
        nonTail(t, tSym => nonTail_*(ts, tSyms => ctx(tSym :: tSyms)))
    }

  private def tail(tree: S.Tree, c: Symbol): C.Tree =
    tree match {
      case S.Let((name, value) :: rest, body) =>
        tempLetC("lc", List(name), tail(S.Let(rest, body), c)) { lc =>
          tail(value, lc) }

      case S.Let(List(), body) =>
        tail(body, c)

      // TODO

    }

  private def cond(tree: S.Tree, trueC: Symbol, falseC: Symbol): C.Tree =
    tree match {
      case S.If(condE, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))) =>
        cond(condE, trueC, falseC)

      case S.If(condE, S.Lit(BooleanLit(false)), S.Lit(BooleanLit(true))) =>
        cond(condE, falseC, trueC)

      // TODO

      case S.Prim(p: L3TestPrimitive, args) =>
        nonTail_*(args, as => C.If(p, as, trueC, falseC))

      case other =>
        nonTail(other, o =>
          nonTail(S.Lit(BooleanLit(false)), n =>
            C.If(L3Ne, List(o, n), trueC, falseC)))
    }

  private def tempLetC(cName: String, args: List[C.Name], cBody: C.Tree)
                      (body: C.Name => C.Tree): C.Tree = {
    val cSym = Symbol.fresh(cName)
    C.LetC(List(C.CntDef(cSym, args, cBody)), body(cSym))
  }
}
