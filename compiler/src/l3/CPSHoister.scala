package l3

import SymbolicCPSTreeModuleLow._

object CPSHoister extends (Tree => Tree) {
  def apply(tree: Tree): Tree =
    hoist(tree)

  private def hoist(tree: Tree): LetF = tree match {
    case LetL(name, value, body) =>
      val LetF(funs, hBody) = hoist(body)
      LetF(funs, LetL(name, value, hBody))
    
    case LetF(functions, body) => 
      val LetF(funs, hBody) = hoist(body)
      LetF(funs ::: functions.flatMap(hoistF(_)), hBody)
    
    case LetP(name, prim, args, body) => 
      val LetF(funs, hBody) = hoist(body)
      LetF(funs, LetP(name, prim, args, hBody))
    
    case LetC(continuations, body) => 
      val LetF(funs, hBody) = hoist(body)
      val ctns = continuations.map(hoistC(_))
      LetF(funs ::: ctns.flatMap(_._1), LetC(ctns.map(_._2), hBody))
    
    case x => LetF(Nil, x)
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
