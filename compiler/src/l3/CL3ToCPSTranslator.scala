package l3

import l3.{ SymbolicCL3TreeModule => S }
import l3.{ SymbolicCPSTreeModule => C }

object CL3ToCPSTranslator extends (S.Tree => C.Tree) {
  def apply(tree: S.Tree): C.Tree = {
    nonTail(tree, _ => C.Halt)
  }

  private def nonTail(tree: S.Tree, ctx: Symbol => C.Tree): C.Tree =
    tree match {
    
      case S.Ident(name) => ctx(name)
      case S.Lit(lit) => {
        val n = Symbol.fresh("n")
        C.LetL(n, lit, ctx(n))
      }
      
      case S.Let((name, value) :: rest, body) =>
        tempLetC("lc", List(name), nonTail(S.Let(rest, body), ctx)) { lc =>
          tail(value, lc) }

      case S.Let(List(), body) =>
        nonTail(body, ctx)
        
      case S.LetRec(functions, body) =>
        C.LetF(functions.map(f => {
          val c = Symbol.fresh("c")
          C.FunDef(f.name, c, f.args, tail(f.body, c))
        }), nonTail(body, ctx))
        
      case S.App(f, args) => {
        val (c, r) = (Symbol.fresh("c"), Symbol.fresh("r"))
        nonTail_*(args, l => C.LetC(List(C.CntDef(c, List(r), ctx(r))), C.AppF(l.head, c, l.tail)))
      }
      
      case S.Prim(name, args) if name.isInstanceOf[L3TestPrimitive] => 
        nonTail(S.If(tree, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))), ctx)
      
      
      // if may not be necassary
      case S.Prim(name, args)  if name.isInstanceOf[L3ValuePrimitive] => {
        val n = Symbol.fresh("n")
        nonTail_*(args, l => C.LetP(n, name.asInstanceOf[L3ValuePrimitive], l, ctx(n)))
      }
      
      case S.If(prim @ S.Prim(name, args), e2, e3) if name.isInstanceOf[L3TestPrimitive] => {
        val r = Symbol.fresh("r")
        tempLetC("lc", List(r), ctx(r)) {
          lc => tempLetC("lct", List(), tail(e2, lc)) {
            lct => tempLetC("lcf", List(), tail(e3, lc)) {
              lcf => nonTail_*(args, l => C.If(name.asInstanceOf[L3TestPrimitive], l, lct, lcf))
            }
          }
        }
      }
      
      /*case S.If(e1, e2, e3) => {
        val r = Symbol.fresh("r")
        tempLetC("lc", List(r), ctx(r)) {
          lc => tempLetC("lct", List(), tail(e2, lc)) {
            lct => tempLetC("lcf", List(), tail(e3, lc)) {
              lcf => {
                val f = Symbol.fresh("f")
                C.LetL(f, BooleanLit(false), nonTail(e1, v => C.If(L3Ne, List(v, f), lct, lcf)))
              }
            }
          }
        }
      }*/
      
      case S.If(e1, e2, e3) => {
        val r = Symbol.fresh("r")
        tempLetC("lc", List(r), ctx(r)) {
          lc => tempLetC("lct", List(), tail(e2, lc)) {
            lct => tempLetC("lcf", List(), tail(e3, lc)) {
              lcf => cond(e1, lct, lcf)
            }
          }
        }
      }
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
      
      case S.Lit(lit) => {
        val l = Symbol.fresh("l")
        C.LetL(l, lit, C.AppC(c, List(l)))
      }
      // Why ?
      case S.App(S.Ident(f), List()) =>
        C.AppF(f, c, List())
      //case S.App(S.Ident(f), x :: xs) => ???
      // TODO

    }

  private def cond(tree: S.Tree, trueC: Symbol, falseC: Symbol): C.Tree =
    tree match {
      case S.If(condE, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))) =>
        cond(condE, trueC, falseC)

      case S.If(condE, S.Lit(BooleanLit(false)), S.Lit(BooleanLit(true))) =>
        cond(condE, falseC, trueC)
      
      /* From here... */
      case S.If(condE, e2, S.Lit(BooleanLit(true))) =>
        tempLetC("ac", List(), cond(e2, trueC, falseC)) {
          ac => cond(condE, ac, trueC)
        }
      
      case S.If(condE, e2, S.Lit(BooleanLit(false))) =>
        tempLetC("ac", List(), cond(e2, trueC, falseC)) {
          ac => cond(condE, ac, falseC)
        }
      
      case S.If(condE, S.Lit(BooleanLit(true)), e3) =>
        tempLetC("ac", List(), cond(e3, trueC, falseC)) {
          ac => cond(condE, trueC, ac)
        }
      
      case S.If(condE, S.Lit(BooleanLit(false)), e3) =>
        tempLetC("ac", List(), cond(e3, trueC, falseC)) {
          ac => cond(condE, falseC, ac)
        }
      /* ...to here, I'm not sure of what I did, all the other stuff was already there */
        
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
