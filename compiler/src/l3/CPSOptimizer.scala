package l3

import scala.collection.mutable.{ Map => MutableMap }

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
      case _ =>
        // TODO:
        tree
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
    // TODO
    case _ => false
  }

  protected def isStable(prim: ValuePrimitive): Boolean = {
    require(!isImpure(prim))
    prim match {
      // TODO
      case _ => true
    }
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

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] =
    Set((0, CPSAdd), (0, CPSOr), (0, CPSXOr),
        (1, CPSMul), (0, CPSAnd))
  protected val rightNeutral: Set[(ValuePrimitive, Literal)] =
    Set((CPSAdd, 0), (CPSSub, 0), (CPSOr, 0), (CPSXOr, 0),
        (CPSMul, 1), (CPSDiv, 1), (CPSAnd, 0),
        (CPSArithShiftL, 0), (CPSArithShiftR, 0))
  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] =
    Set((0, CPSMul), (0, CPSAnd))
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] =
    Set((CPSMul, 0), (CPSAnd, 0))
  protected val sameArgReduce: Map[ValuePrimitive, Literal] =
    Map(CPSSub -> 0)

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
