package l3

import BitTwiddling.bitsToIntMSBF
import l3.{ SymbolicCPSTreeModule => H }
import l3.{ SymbolicCPSTreeModuleLow => L }

/**
 * Data-representation phase for the CPS language. Translates a tree
 * with high-level datatypes (blocks, integers, booleans, unit) and
 * corresponding primitives to one with low-level datatypes (blocks
 * and integers only) and corresponding primitives.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object CPSDataRepresenter extends (H.Tree => L.Tree) {
  def apply(tree: H.Tree): L.Tree = 
    transform(tree)
    
  private def freeVars(tree: H.Tree): List[H.Name] = tree match {
    case H.LetL(name, value, body) => freeVars(body).filter(_ != name)
    case H.LetP(name, prim, args, body) => freeVars(body).filter(_ != name) ::: args
    case H.LetC(continuations, body) => freeVars(body) ::: continuations.flatMap(c => freeVars(c.body).diff(c.args))
    case H.LetF(functions, body) => (freeVars(body) ::: functions.flatMap(f => freeVars(f.body).diff(f.args))).diff(functions.map(f => f.name))
    case H.AppC(cont, args) => args
    case H.AppF(func, cont, args) => func :: args
    case H.If(prim, args, thenC, elseC) => args
    case _ => ???
  }

  private def transform(tree: H.Tree) : L.Tree = tree match {
    // Literals
    case H.LetL(name, IntLit(value), body) =>
      L.LetL(name, (value << 1) | bitsToIntMSBF(1), transform(body))
    case H.LetL(name, CharLit(value), body) =>
      L.LetL(name, (value << 3) | bitsToIntMSBF(1, 1, 0), transform(body))
    case H.LetL(name, BooleanLit(value), body) =>
      L.LetL(name, bitsToIntMSBF((if (value) 1 else 0), 1, 0, 1, 0), transform(body))
    case H.LetL(name, UnitLit, body) =>
      L.LetL(name, bitsToIntMSBF(0, 0, 1, 0), transform(body))
     
      
    case H.LetP(name, L3IntAdd, args, body) =>
      tempLetP(CPSAdd, args) { r =>
        tempLetL(1) { c1 =>
          L.LetP(name, CPSSub, List(r, c1), transform(body)) } }
    
    case H.LetP(name, L3IntSub, args, body) =>
      tempLetP(CPSSub, args) { r =>
        tempLetL(1) { c1 =>
          L.LetP(name, CPSAdd, List(r, c1), transform(body)) }}
    
    case H.LetP(name, L3IntMul, args, body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSSub, List(args(0), c1)) { r1 =>
          tempLetP(CPSArithShiftR, List(args(1), c1)) { r2 => 
            tempLetP(CPSMul, List(r1, r2)) { r =>
              L.LetP(name, CPSAdd, List(r, c1), transform(body)) }}}}
    
    case H.LetP(name, L3IntDiv, args, body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSSub, List(args(0), c1)) { r1 =>
          tempLetP(CPSSub, List(args(1), c1)) { r2 => 
            tempLetP(CPSDiv, List(r1, r2)) { r3 =>
              tempLetL(2) { c2 => 
                tempLetP(CPSMul, List(r3, c2)) { r => 
                  L.LetP(name, CPSAdd, List(r, c1), transform(body)) }}}}}}
    
    case H.LetP(name, L3IntMod, args, body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSSub, List(args(0), c1)) { r1 =>
          tempLetP(CPSSub, List(args(1), c1)) { r2 => 
            tempLetP(CPSMod, List(r1, r2)) { r =>
              L.LetP(name, CPSAdd, List(r, c1), transform(body)) }}}}
    
    case H.LetP(name, L3IntArithShiftRight, args, body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSSub, List(args(0), c1)) { r1 =>
          tempLetP(CPSSub, List(args(1), c1)) { r2 => 
            tempLetP(CPSArithShiftR, List(r2, c1)) { r3 =>
              tempLetP(CPSArithShiftR, List(r1, r3)) { r =>
                L.LetP(name, CPSAdd, List(r, c1), transform(body)) }}}}}
    
    case H.LetP(name, L3IntArithShiftLeft, args, body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSSub, List(args(0), c1)) { r1 =>
          tempLetP(CPSSub, List(args(1), c1)) { r2 => 
            tempLetP(CPSArithShiftR, List(r2, c1)) { r3 =>
              tempLetP(CPSArithShiftL, List(r1, r3)) { r =>
                L.LetP(name, CPSAdd, List(r, c1), transform(body)) }}}}}
      
    case H.LetP(name, L3IntBitwiseAnd, args, body) =>
      L.LetP(name, CPSAnd, List(args(0), args(1)), transform(body))
    
    case H.LetP(name, L3IntBitwiseOr, args, body) =>
      L.LetP(name, CPSOr, List(args(0), args(1)), transform(body))
      
    case H.LetP(name, L3IntBitwiseXOr, args, body) =>
      tempLetP(CPSXOr, List(args(0), args(1))) { r =>
        tempLetL(1) { c1 => 
          L.LetP(name, CPSOr, List(r, c1), transform(body)) }}
      
    case H.LetF(functions, body) => {
      var consts: List[(L.Name, Int)] = Nil
      def getConst(i: Int): L.Name = consts.filter(_._2 == i) match {
        case x :: Nil => x._1
        case Nil => {
          val c = Symbol.fresh("c")
          consts ::= (c, i)
          c
        }
      }
      
      var pBindings: List[(L.Name, CPSValuePrimitive, List[L.Name])] = Nil
      
      val funDefs = functions.map(f => {
        var innerConsts: List[(L.Name, Int)] = Nil
        def getInnerConst(i: Int): L.Name = innerConsts.filter(_._2 == i) match {
          case x :: Nil => x._1
          case Nil => {
            val c = Symbol.fresh("ci")
            innerConsts ::= (c, i)
            c
          }
        }
        
        val w = Symbol.fresh("w")
        val env = Symbol.fresh("env")
        val fv = freeVars(f.body).distinct.filter(x => x != f.name && !f.args.contains(x))
        val fvBindings = fv.zipWithIndex.map(x => (Symbol.fresh("v"), CPSBlockGet, List(env, getInnerConst(x._2+1))))
        
        pBindings ::= (f.name, CPSBlockAlloc(202), List(getConst(fv.size+1)))
        
        var i = 1
        pBindings ::= (Symbol.fresh("t"), CPSBlockSet, List(f.name, getConst(0), w))
        fv.foreach(v => {
          pBindings ::= (Symbol.fresh("t"), CPSBlockSet, List(f.name, getConst(i), v))
          i = i + 1
        })
        
        L.FunDef(w, f.retC, env :: f.args,
          LetL_*(innerConsts)(
            LetP_*(fvBindings)(
              transform(f.body.subst(PartialFunction[L.Name, L.Name] {
                case x if x == f.name => env
                case x if fv.contains(x) => fv.zip(fvBindings).filter(_._1 == x)(0)._2._1
                case x => x
              })))))
      })

      LetL_*(consts)(
        L.LetF(funDefs, 
          LetP_*(pBindings.reverse)(transform(body))))
    }
    
    case H.AppF(fun, retC, args) => {
      val f = Symbol.fresh("f")
      tempLetL(0) { zero =>
        L.LetP(f, CPSBlockGet, List(fun, zero), L.AppF(f, retC, fun :: args))
      }
    }
      /*L.LetF(functions.map(f => 
        L.FunDef(f.name, f.retC, f.args, transform(f.body))),
        transform(body)
      )*/
      
    // Casts
    case H.LetP(name, L3IntToChar, List(a), body) =>
      tempLetL(2) { c2 => 
        tempLetP(CPSArithShiftL, List(a, c2)) { r => 
          L.LetP(name, CPSAdd, List(r, c2), transform(body)) }}
    
    case H.LetP(name, L3CharToInt, List(a), body) =>
      tempLetL(2) { c2 => 
        L.LetP(name, CPSArithShiftR, List(a, c2), transform(body)) }
      
    // Char print
    case H.LetP(name, L3CharPrint, List(a), body) =>
      tempLetL(3) { c3 => 
        tempLetP(CPSArithShiftR, List(a, c3)) { r => 
          L.LetP(name, CPSCharPrint, List(r), transform(body)) }}
    
    // Block primitives
    case H.LetP(name, L3BlockAlloc(tag), List(a), body) =>
      tempLetL(1) { c1 => 
        tempLetP(CPSArithShiftR, List(a, c1)) { r => 
          L.LetP(name, CPSBlockAlloc(tag), List(r), transform(body)) }}
      
    case H.LetP(name, L3BlockTag, List(a), body) =>
      tempLetP(CPSBlockTag, List(a)) { r1 => 
        tempLetL(1) { c1 => 
          tempLetP(CPSArithShiftL, List(r1, c1)) { r2 => 
            L.LetP(name, CPSAdd, List(r2, c1), transform(body)) }}}
        
    case H.LetP(name, L3BlockLength, List(a), body) =>
      tempLetP(CPSBlockSize, List(a)) { r1 => 
        tempLetL(1) { c1 => 
          tempLetP(CPSArithShiftL, List(r1, c1)) { r2 => 
            L.LetP(name, CPSAdd, List(r2, c1), transform(body)) }}}
      
    case H.LetP(name, L3BlockGet, List(b, n), body) =>
      tempLetL(1) { c1 => 
        tempLetP(CPSArithShiftR, List(n, c1)) { r => 
          L.LetP(name, CPSBlockGet, List(b, r), transform(body)) }}
      
    case H.LetP(name, L3BlockSet, List(b, n, v), body) =>
      tempLetL(1) { c1 => 
        tempLetP(CPSArithShiftR, List(n, c1)) { r => 
          L.LetP(name, CPSBlockSet, List(b, r, v), transform(body)) }}
    
    case H.LetC(continuations, body) => L.LetC(continuations.map(c => L.CntDef(c.name, c.args, transform(c.body))), transform(body))
    
    case H.AppC(c, args) => L.AppC(c, args)
      
    case H.If(L3IntLt, args, thenC, elseC) =>
      L.If(CPSLt, args, thenC, elseC)
    case H.If(L3IntLe, args, thenC, elseC) =>
      L.If(CPSLe, args, thenC, elseC)
    case H.If(L3IntGt, args, thenC, elseC) =>
      L.If(CPSGt, args, thenC, elseC)
    case H.If(L3IntGe, args, thenC, elseC) =>
      L.If(CPSGe, args, thenC, elseC)
    case H.If(L3Eq, args, thenC, elseC) =>
      L.If(CPSEq, args, thenC, elseC)
    case H.If(L3Ne, args, thenC, elseC) =>
      L.If(CPSNe, args, thenC, elseC)
      
    case H.If(L3IntP, List(a), thenC, elseC) =>
      ifEqLSB(a, List(1), thenC, elseC)
    case H.If(L3BlockP, List(a), thenC, elseC) =>
      ifEqLSB(a, List(0, 0), thenC, elseC)
    case H.If(L3CharP, List(a), thenC, elseC) =>
      ifEqLSB(a, List(1, 1, 0), thenC, elseC)
    case H.If(L3BoolP, List(a), thenC, elseC) =>
      ifEqLSB(a, List(1, 0, 1, 0), thenC, elseC)
    case H.If(L3UnitP, List(a), thenC, elseC) =>
      ifEqLSB(a, List(0, 0, 1, 0), thenC, elseC)
    
    case H.Halt => L.Halt
    //TODO: handle other cases
  }
  
  private def LetL_*(vars: List[(L.Name, Int)])(body: L.Tree): L.Tree = 
    vars match {
      case Nil =>
        body
      case (c, v) :: vs =>
        L.LetL(c, v, LetL_*(vs)(body))
    }
  
  private def LetP_*(bindings: List[(L.Name, CPSValuePrimitive, List[L.Name])])(body: L.Tree): L.Tree = 
    bindings match {
      case Nil =>
        body
      case (name, prim, args) :: xs =>
        L.LetP(name, prim, args, LetP_*(xs)(body))
    }
  
  // Tree builders

  /**
   * Call body with a fresh name, and wraps its resulting tree in one
   * that binds the fresh name to the given literal value.
   */
  private def tempLetL(v: Int)(body: L.Name => L.Tree): L.Tree = {
    val tempSym = Symbol.fresh("t")
    L.LetL(tempSym, v, body(tempSym))
  }

  /**
   * Call body with a fresh name, and wraps its resulting tree in one
   * that binds the fresh name to the result of applying the given
   * primitive to the given arguments.
   */
  private def tempLetP(p: L.ValuePrimitive, args: List[L.Name])
                      (body: L.Name => L.Tree): L.Tree = {
    val tempSym = Symbol.fresh("t")
    L.LetP(tempSym, p, args, body(tempSym))
  }

  /**
   * Generate an If tree to check whether the least-significant bits
   * of the value bound to the given name are equal to those passed as
   * argument. The generated If tree will apply continuation tC if it
   * is the case, and eC otherwise. The bits should be ordered with
   * the most-significant one first (e.g. the list (1,1,0) represents
   * the decimal value 6).
   */
  private def ifEqLSB(arg: L.Name, bits: List[Int], tC: L.Name, eC: L.Name)
      : L.Tree =
    tempLetL(bitsToIntMSBF(bits map { b => 1 } : _*)) { mask =>
      tempLetP(CPSAnd, List(arg, mask)) { masked =>
        tempLetL(bitsToIntMSBF(bits : _*)) { value =>
          L.If(CPSEq, List(masked, value), tC, eC) } } }
}
