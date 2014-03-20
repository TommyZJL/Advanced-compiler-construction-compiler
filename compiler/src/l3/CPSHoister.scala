package l3

import SymbolicCPSTreeModuleLow._

object CPSHoister extends (Tree => Tree) {
  def apply(tree: Tree): Tree =
    hoist(tree)

  private def hoist(tree: Tree): LetF = tree match {
    case LetL(name, value, body) =>
      val LetF(funs, hBody) = hoist(body)
      LetF(funs, LetL(name, value, hBody))
    
    case LetF(functions, body) => ???
    
    case LetP(name, prim, args, tree) => ???
    
    case LetC(continuations, body) => ???
    
    case AppC(cont, args) => ???
    
    case AppF(func, retC, args) => ???
    
    case If(prim, args, thenC, elseC) => ???
    
    case Halt => ???
    //TODO: handle other cases
  }

  private def hoistC(cnt: CntDef): (List[FunDef], CntDef) = {
    val LetF(funs, hBody) = hoist(cnt.body)
    (funs, CntDef(cnt.name, cnt.args, hBody))
  }

  private def hoistF(fun: FunDef): List[FunDef] = {
    val LetF(funs, hBody) = hoist(fun.body)
    FunDef(fun.name, fun.retC, fun.args, hBody) :: funs
  }
}
