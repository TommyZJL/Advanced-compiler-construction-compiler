package l3

import l3.{ SymbolicCL3TreeModule => S }
import l3.{ SymbolicCPSTreeModule => C }

object CL3ToCPSTranslator extends (S.Tree => C.Tree) {
  def apply(tree: S.Tree): C.Tree = {
    nonTail(tree, _ => C.Halt)
  }

  private def nonTail(tree: S.Tree, ctx: Symbol => C.Tree): C.Tree = tree match {
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
      
    case S.LetRec(functions, body) => {
      C.LetF(functions.map(f => {
        val c = Symbol.fresh("c")
        C.FunDef(f.name, c, f.args, tail(f.body, c))
      }), nonTail(body, ctx))
    }
    
    case S.App(f, args) => {
      val (c, r) = (Symbol.fresh("c"), Symbol.fresh("r"))
      nonTail_*(f :: args, l => 
        C.LetC(List(C.CntDef(c, List(r), ctx(r))), C.AppF(l.head, c, l.tail)))
    }
    
    case S.Prim(name: L3TestPrimitive, args) => 
      nonTail(S.If(tree, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))), ctx)
    
    case S.Prim(name: L3ValuePrimitive, args) => {
      val n = Symbol.fresh("n")
      nonTail_*(args, l => C.LetP(n, name.asInstanceOf[L3ValuePrimitive], l, ctx(n)))
    }
    
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

  private def nonTail_*(trees: List[S.Tree], ctx: List[Symbol] => C.Tree): C.Tree = 
    trees match {
      case List() =>
        ctx(List())
      case t :: ts =>
        nonTail(t, tSym => nonTail_*(ts, tSyms => ctx(tSym :: tSyms)))
    }

  private def tail(tree: S.Tree, c: Symbol): C.Tree =
    tree match {
      case S.Let((name, value) :: rest, body) =>
        tempLetC("lc", List(name), tail(S.Let(rest, body), c)) { 
          lc => tail(value, lc) 
        }

      case S.Let(List(), body) =>
        tail(body, c)
      
      case S.Ident(name) => 
        C.AppC(c, List(name))
        
      case S.Lit(lit) => {
        val l = Symbol.fresh("l")
        C.LetL(l, lit, C.AppC(c, List(l)))
      }
        
      case S.LetRec(functions, body) =>
        C.LetF(functions.map(f => {
          val c = Symbol.fresh("c")
          C.FunDef(f.name, c, f.args, tail(f.body, c))
        }), tail(body, c))
      
      case S.App(f, l) =>
        nonTail_*(f :: l, list => C.AppF(list.head, c, list.tail))
        
      case S.Prim(name: L3TestPrimitive, args) => 
        tail(S.If(tree, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))), c)
      
      case S.Prim(name: L3ValuePrimitive, args) => {
        val n = Symbol.fresh("n")
        nonTail_*(args, l => C.LetP(n, name, l, C.AppC(c, List(n))))
      }
      
      case S.If(e1, e2, e3) => {
        tempLetC("lct", List(), tail(e2, c)) {
          lct => tempLetC("lcf", List(), tail(e3, c)) {
            lcf => cond(e1, lct, lcf)
          }
        }
      }
      
      case _ => 
        nonTail(tree, t => C.AppC(c, List(t)))
    }

  private def cond(tree: S.Tree, trueC: Symbol, falseC: Symbol): C.Tree =
    tree match {
      case S.If(condE, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))) =>
        cond(condE, trueC, falseC)

      case S.If(condE, S.Lit(BooleanLit(false)), S.Lit(BooleanLit(true))) =>
        cond(condE, falseC, trueC)
      
      case S.If(condE, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(true))) =>
        cond(condE, trueC, trueC)

      case S.If(condE, S.Lit(BooleanLit(false)), S.Lit(BooleanLit(false))) =>
        cond(condE, falseC, falseC)
        
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
