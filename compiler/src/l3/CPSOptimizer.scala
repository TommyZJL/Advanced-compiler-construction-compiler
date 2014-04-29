package l3

import scala.collection.mutable.{Map => MutableMap}

abstract class CPSOptimizer[T <: CPSTreeModule { type Name = Symbol }]
  (val treeModule: T) {
  import treeModule._

  def apply(tree: Tree): Tree = {
    val simplifiedTree = fixedPoint(tree)(shrink)
    val maxSize = (size(simplifiedTree) * 1.5).toInt
    fixedPoint(simplifiedTree, 8) { t => shrink(inline(t, maxSize)) }
  }

  // Usage count, refined into function applications and usage as value
  private case class Count(applied: Int = 0, asValue: Int = 0)

  // State used in transformations:
  private case class State(
    census: Map[Name, Count],
    subst: Substitution[Name] = Substitution.empty,
    lEnv: Map[Name, Literal] = Map.empty,
    lInvEnv: Map[Literal, Name] = Map.empty,
    eInvEnv: Map[(ValuePrimitive, List[Name]), Name] = Map.empty,
    cEnv: Map[Name, CntDef] = Map.empty,
    fEnv: Map[Name, FunDef] = Map.empty) {

    def dead(s: Name): Boolean =
      census get s map (_ == Count(applied = 0, asValue = 0)) getOrElse true
    def appliedOnce(s: Name): Boolean =
      census get s map (_ == Count(applied = 1, asValue = 0)) getOrElse false

    def withSubst(from: Name, to: Name): State =
      copy(subst = subst + (from -> to))
    def withSubst(from: Seq[Name], to: Seq[Name]): State = {
      require(from.length == to.length)
      copy(subst = subst ++ (from zip to))
    }

    def withLit(name: Name, value: Literal) =
      copy(lEnv = lEnv + (name -> value), lInvEnv = lInvEnv + (value -> name))
    def withExp(name: Name, prim: ValuePrimitive, args: List[Name]) =
      copy(eInvEnv = eInvEnv + ((prim, args) -> name))
    def withCnt(cnt: CntDef) =
      copy(cEnv = cEnv + (cnt.name -> cnt))
    def withCnts(cnts: Seq[CntDef]) =
      (this /: cnts) (_.withCnt(_))
    def withFun(fun: FunDef) =
      copy(fEnv = fEnv + (fun.name -> fun))
    def withFuns(funs: Seq[FunDef]) =
      (this /: funs) (_.withFun(_))
    def withEmptyInvEnvs =
      copy(lInvEnv = Map.empty, eInvEnv = Map.empty)
  }

  // Shrinking optimizations:
  private def shrink(tree: Tree): Tree = {
    def shrinkT(tree: Tree)(implicit s: State): Tree = tree match {
      // Literals
      case LetL(name, value, body) if s.dead(name) => shrinkT(body)
      case LetL(name, value, body) if s.lInvEnv.contains(value) && name != s.lInvEnv.get(value).get =>
        shrinkT(body.subst(PartialFunction[Name, Name] {
          case x if x == name => s.lInvEnv.get(value).get
          case x => x
        }))//(s.withSubst(name, s.lInvEnv.get(value).get))
      
      // Primitives
      case LetP(name, prim, args, body) if s.dead(name) && !isImpure(prim) &&
        !hasBorderEffect(prim) => {
          shrinkT(body)
        }
      case LetP(name, prim, args, body) if s.eInvEnv.contains((prim, args)) 
          && name != s.eInvEnv.get((prim, args)).get =>
        shrinkT(body.subst(PartialFunction[Name, Name] {
          case x if x == name => s.eInvEnv.get((prim,args)).get
          case x => x
        }))
      case LetP(name, prim, List(b, i), body) 
        if (s.eInvEnv.map(x => (x._2, x._1)).get(b) match {
          case Some(x) => isFixedBlockAlloc(x._1)
          case _ => false
        }) && (s.eInvEnv.filter(x => x._1._1 == blockSet && x._1._2(0) == b && x._1._2(1) == i).size == 1) => {
          val tuple = s.eInvEnv.filter(x => x._1._1 == blockSet && x._1._2(0) == b && x._1._2(1) == i).toList(0)
          val v = tuple._1._2(2)
          
          shrinkT(body.subst(PartialFunction[Name, Name] {
            case x if x == name => v
            case x => x
          }))
        }
        
      case LetP(name, prim, args, body) if args.forall(a => s.lEnv.contains(a)) 
            && vEvaluator.isDefinedAt((prim, args.map(a => s.lEnv.get(a).get))) => { 
        val value = vEvaluator.apply((prim, args.map(a => s.lEnv.get(a).get)))
        LetL(name, value, shrinkT(body)(s.withLit(name, value)))
      }
      
      // Left/Right absorbing/neutral
      case LetP(name, prim, args, body) if args.exists(a => s.lEnv.contains(a) &&
          (leftAbsorbing.contains((s.lEnv.get(a).get, prim)) || rightAbsorbing.contains((prim, s.lEnv.get(a).get))
              || leftNeutral.contains((s.lEnv.get(a).get, prim)) || rightNeutral.contains((prim, s.lEnv.get(a).get)))) => {
        val value = args.filter(a => s.lEnv.contains(a) &&
          (leftAbsorbing.contains((s.lEnv.get(a).get, prim)) || rightAbsorbing.contains((prim, s.lEnv.get(a).get))
              || leftNeutral.contains((s.lEnv.get(a).get, prim)) || rightNeutral.contains((prim, s.lEnv.get(a).get))))
        shrinkT(body)(s.withSubst(name, value.head))
      }
      
      // If
      case If(cond, args, thenC, elseC) if args.forall(a => s.lEnv.contains(a)) 
            && cEvaluator.isDefinedAt((cond, args.map(a => s.lEnv.get(a).get))) => {
        val value = cEvaluator.apply((cond, args.map(a => s.lEnv.get(a).get)))
        if (value) {
          AppC(thenC, Nil)
        }
        else {
          AppC(elseC, Nil)
        }
      }
      
      // Functions
      case LetF(functions, body) => {
        functions.filter(f => {
          functions.filter(_ != f).forall(f2 =>
            !census(f2.body).contains(f.name) || census(body).contains(f.name)
          )
        }) match {
          case Nil => shrinkT(body)
          case x::xs => LetF(x::xs, shrinkT(body))
        }
      }
      
      // Continuations
      case LetC(continuations, body) => {
        continuations.filter(c => {
          continuations.filter(_ != c).forall(c2 =>
            !census(c2.body).contains(c.name) || census(body).contains(c.name)
          )
        }) match {
          case Nil => shrinkT(body)
          case x::xs => LetC(continuations.map(x => CntDef(x.name, x.args, shrinkT(x.body))), shrinkT(body))
        }
      }
      
      // Base cases 
      case LetL(name, value, body) =>
        LetL(name, value, shrinkT(body)(s.withLit(name, value)))
      case LetP(name, prim, args, body) =>
        LetP(name, prim, args, shrinkT(body)(s.withExp(name, prim, args)))
      case x =>
        x
    }

    shrinkT(tree)(State(census(tree)))
  }

  // (Non-shrinking) inlining:
  private def inline(tree: Tree, maxSize: Int): Tree = {

    val fibonacci = Seq(1, 2, 3, 5, 8, 13)

    val trees = Stream.iterate((0, tree), fibonacci.length) { case (i, tree) =>
      val funLimit = fibonacci(i)
      val cntLimit = i

      def inlineT(tree: Tree)(implicit s: State): Tree = tree match {
        case _ =>
          // TODO:
          tree
      }

      (i + 1, inlineT(tree)(State(census(tree))))
    }

    (trees takeWhile { case (_, tree) => size(tree) <= maxSize }).last._2
  }

  // Census computation, counts how many times each symbol is used
  private def census(tree: Tree): Map[Name, Count] = {
    val census = MutableMap[Name, Count]()
    val rhs = MutableMap[Name, Tree]()

    def incAppUse(symbol: Name): Unit = {
      val currCount = census.getOrElse(symbol, Count())
      census(symbol) = currCount.copy(applied = currCount.applied + 1)
      rhs remove symbol foreach addToCensus
    }

    def incValUse(symbol: Name): Unit = {
      val currCount = census.getOrElse(symbol, Count())
      census(symbol) = currCount.copy(asValue = currCount.asValue + 1)
      rhs remove symbol foreach addToCensus
    }

    def addToCensus(tree: Tree): Unit = (tree: @unchecked) match {
      case LetL(_, _, body) =>
        addToCensus(body)
      case LetP(_, _, args, body) =>
        args foreach incValUse; addToCensus(body)
      case LetC(cnts, body) =>
        rhs ++= (cnts map { c => (c.name, c.body) }); addToCensus(body)
      case LetF(funs, body) =>
        rhs ++= (funs map { f => (f.name, f.body) }); addToCensus(body)
      case AppC(cont, args) =>
        incAppUse(cont); args foreach incValUse
      case AppF(fun, retC, args) =>
        incAppUse(fun); incValUse(retC); args foreach incValUse
      case If(_, args, thenC, elseC) =>
        args foreach incValUse; incValUse(thenC); incValUse(elseC)
      case Halt =>
        ()
    }

    addToCensus(tree)
    census.toMap
  }

  private def size(tree: Tree): Int = (tree: @unchecked) match {
    case LetL(_, _, body) => size(body) + 1
    case LetP(_, _, _, body) => size(body) + 1
    case LetC(cs, body) => (cs map { c => size(c.body) }).sum + size(body)
    case LetF(fs, body) => (fs map { f => size(f.body) }).sum + size(body)
    case AppC(_, _) | AppF(_, _, _) | If(_, _, _, _) | Halt => 1
  }

  protected def isImpure(prim: ValuePrimitive): Boolean
  protected def isStable(prim: ValuePrimitive): Boolean
  
  protected def blockSet(): ValuePrimitive
  protected def isFixedBlockAlloc(prim: ValuePrimitive): Boolean
  protected def hasBorderEffect(prim: ValuePrimitive): Boolean
  
  protected val leftNeutral: Set[(Literal, ValuePrimitive)]
  protected val rightNeutral: Set[(ValuePrimitive, Literal)]
  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)]
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)]
  protected val sameArgReduce: Map[ValuePrimitive, Literal]
  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal]
  protected val cEvaluator: PartialFunction[(TestPrimitive, List[Literal]),
                                            Boolean]
}

object CPSOptimizerHigh extends CPSOptimizer(SymbolicCPSTreeModule)
    with (SymbolicCPSTreeModule.Tree => SymbolicCPSTreeModule.Tree) {
  import treeModule._

  protected def isImpure(prim: ValuePrimitive): Boolean = prim match {
    case L3BlockSet | L3CharRead | L3CharPrint => true
    case _ => false
  }

  protected def isStable(prim: ValuePrimitive): Boolean = {
    require(!isImpure(prim))
    prim match {
      case L3BlockAlloc(_) | L3BlockGet => false
      case _ => true
    }
  }
  
  protected def blockSet: ValuePrimitive = L3BlockSet
  
  protected def isFixedBlockAlloc(prim: ValuePrimitive): Boolean = prim match {
    case L3BlockAlloc(x) if x == 202 => true
    case _ => false
  }
  
  protected def hasBorderEffect(prim: ValuePrimitive): Boolean = prim match {
    case L3IntDiv | L3IntMod => true
    case _ => false
  }

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] =
    Set() // TODO
  protected val rightNeutral: Set[(ValuePrimitive, Literal)] =
    Set() // TODO
  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] =
    Set() // TODO
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] =
    Set() // TODO
  protected val sameArgReduce: Map[ValuePrimitive, Literal] =
    Map() // TODO

  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal] = {
    case (L3IntAdd, Seq(IntLit(x), IntLit(y))) => IntLit(x + y)
    // TODO
  }

  protected val cEvaluator: PartialFunction[(TestPrimitive, List[Literal]),
                                            Boolean] = {
    case (L3IntP, Seq(IntLit(_))) => true
    // TODO
  }
}

object CPSOptimizerLow extends CPSOptimizer(SymbolicCPSTreeModuleLow)
    with (SymbolicCPSTreeModuleLow.Tree => SymbolicCPSTreeModuleLow.Tree) {
  import treeModule._

  protected def isImpure(prim: ValuePrimitive): Boolean = prim match {
    case CPSBlockSet | CPSCharRead | CPSCharPrint => true
    case _ => false
  }

  protected def isStable(prim: ValuePrimitive): Boolean = {
    require(!isImpure(prim))
    prim match {
      case CPSBlockAlloc(_) | CPSBlockGet => false
      case _ => true
    }
  }
  
  protected def blockSet: ValuePrimitive = CPSBlockSet
  
  protected def isFixedBlockAlloc(prim: ValuePrimitive): Boolean = prim match {
    case CPSBlockAlloc(x) if x == 202 => true
    case _ => false
  }
  
  protected def hasBorderEffect(prim: ValuePrimitive): Boolean = prim match {
    case CPSDiv | CPSMod => true
    case _ => false
  }

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] =
    Set((0, CPSAdd), (0, CPSOr), (0, CPSXOr),
        (1, CPSMul))
  protected val rightNeutral: Set[(ValuePrimitive, Literal)] =
    Set((CPSAdd, 0), (CPSSub, 0), (CPSOr, 0), (CPSXOr, 0),
        (CPSMul, 1), (CPSDiv, 1),
        (CPSArithShiftL, 0), (CPSArithShiftR, 0))
  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] =
    Set((0, CPSMul), (0, CPSAnd))
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] =
    Set((CPSMul, 0), (CPSAnd, 0))
  protected val sameArgReduce: Map[ValuePrimitive, Literal] =
    Map(CPSSub -> 0, CPSDiv -> 1, CPSMod -> 0, CPSXOr -> 0)

  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal] = {

    case (CPSAdd, Seq(x, y)) => x + y
    case (CPSSub, Seq(x, y)) => x - y
    case (CPSMul, Seq(x, y)) => x * y
    case (CPSDiv, Seq(x, y)) if (y != 0) => x / y
    case (CPSMod, Seq(x, y)) if (y != 0) => x % y

    case (CPSArithShiftL, Seq(x, y)) => x << y
    case (CPSArithShiftR, Seq(x, y)) => x >> y
    case (CPSAnd, Seq(x, y)) => x & y
    case (CPSOr, Seq(x, y)) => x | y
    case (CPSXOr, Seq(x, y)) => x ^ y
  }

  protected val cEvaluator: PartialFunction[(TestPrimitive, List[Literal]),
                                            Boolean] = {

    case (CPSLt, Seq(x, y)) => x < y
    case (CPSLe, Seq(x, y)) => x <= y
    case (CPSEq, Seq(x, y)) => x == y
    case (CPSNe, Seq(x, y)) => x != y
    case (CPSGe, Seq(x, y)) => x >= y
    case (CPSGt, Seq(x, y)) => x > y
  }
}
