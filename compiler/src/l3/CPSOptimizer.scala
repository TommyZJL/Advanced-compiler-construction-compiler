package l3

import scala.collection.mutable.{Map => MutableMap}
import java.io.PrintWriter
import java.io.PrintStream
import java.io.BufferedOutputStream
import scala.Console

abstract class CPSOptimizer[T <: CPSTreeModule { type Name = Symbol }]
  (val treeModule: T) {
  import treeModule._

  def apply(tree: Tree): Tree = {
    //printTree("Tree to optimize :", tree)
    val simplifiedTree = fixedPoint(tree)(shrink)
    val maxSize = (size(simplifiedTree) * 1.5).toInt
    val ft = fixedPoint(simplifiedTree, 8) { t => shrink(inline(t, maxSize)) }
    //printTree("Final Tree : ", ft)
    ft
  }
  
  private def printTree(title: String, tree: Tree): Unit = {
    val w = new PrintWriter(Console.out)
    w.println(title)
    new CPSTreeFormatter[T](treeModule).toDocument(tree).format(80, w)
    w.println()
    w.flush()
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
    //printTree("Tree to shrink :", tree)
    
    def shrinkT(tree: Tree)(implicit s: State): Tree = tree match {
      // Literals
      case LetL(name, value, body) if s.dead(name) => 
        shrinkT(body)
      case LetL(name, value, body) if s.lInvEnv.contains(value) && name != s.lInvEnv.get(value).get =>
        shrinkT(body.subst(PartialFunction[Name, Name] {
          case x if x == name => s.lInvEnv.get(value).get
          case x => x
        }))
      
      case LetL(name, value, body) =>
        LetL(name, value, shrinkT(body)(s.withLit(name, value)))
      
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
      case LetP(name, prim, args @ List(b, i), body)
        if isBlockGet(prim) && (s.eInvEnv.map(x => (x._2, x._1)).get(b) match {
          case Some(x) => isFixedBlockAlloc(x._1)
          case _ => false
        }) => {
          s.eInvEnv.filter(x => x._1._1 == blockSet && x._1._2(0) == b && x._1._2(1) == i).toList match {
            case x :: Nil =>
              val v = x._1._2(2)
          
              shrinkT(body.subst(PartialFunction[Name, Name] {
                case x if x == name => v
                case x => x
              }))
            case _ => LetP(name, prim, args, shrinkT(body)(s.withExp(name, prim, args)))
          }
        }
        
      // Same args reduce
      case LetP(name, prim, args, body) if sameArgReduce.contains(prim) && args(0) == args(1)  => {
        val value = sameArgReduce.get(prim).get
        LetL(name, value, shrinkT(body)(s.withLit(name, value)))
      }
      // Left/Right absorbing/neutral
      case LetP(name, prim, args, body) if args.exists(a => s.lEnv.contains(a) &&
          (leftAbsorbing.contains((s.lEnv.get(a).get, prim)) || rightAbsorbing.contains((prim, s.lEnv.get(a).get))
              || leftNeutral.contains((s.lEnv.get(a).get, prim)) || rightNeutral.contains((prim, s.lEnv.get(a).get)))) => {
        
        val value = args.filter(a => s.lEnv.contains(a) &&
          (leftAbsorbing.contains((s.lEnv.get(a).get, prim)) || rightAbsorbing.contains((prim, s.lEnv.get(a).get))
              || leftNeutral.contains((s.lEnv.get(a).get, prim)) || rightNeutral.contains((prim, s.lEnv.get(a).get))))
        
        args match {
          case List(x, y) if x == value.head => shrinkT(body.subst(PartialFunction[Name, Name] {
            case n if n == name => y
            case n => n
          }))
          case List(x, y) if y == value.head => shrinkT(body.subst(PartialFunction[Name, Name] {
            case n if n == name => x
            case n => n
          }))
          case _ => shrinkT(body) // Should never happen
        }
      }
      
      case LetP(name, prim, args, body) if args.forall(a => s.lEnv.contains(a)) 
            && vEvaluator.isDefinedAt((prim, args.map(a => s.lEnv.get(a).get))) => { 
        val value = vEvaluator((prim, args.map(a => s.lEnv.get(a).get)))
        LetL(name, value, shrinkT(body)(s.withLit(name, value)))
      }
      
      case LetP(name, prim, args, body) => prim match {
        case x if isBlockLength(x) => 
          s.eInvEnv.map(x => (x._2, x._1)).get(args(0)) match {
            case Some(y) =>
              shrinkT(body.subst(PartialFunction[Name, Name] {
                case x if x == name => y._2(0)
                case x => x
              }))
            case None => LetP(name, prim, args, shrinkT(body)(s.withExp(name, prim, args)))
        }
        case x if isBlockTag(x) => 
          s.eInvEnv.map(x => (x._2, x._1)).get(args(0)) match {
            case Some(x) =>
              val fresh = Symbol.fresh("tag")
              LetL(fresh,getBlockTag(x._1), shrinkT(body.subst(PartialFunction[Name, Name] {
                case x if x == name => fresh
                case x => x
              }))(s.withLit(fresh, getBlockTag(x._1))))
            case None => LetP(name, prim, args, shrinkT(body)(s.withExp(name, prim, args)))
          }
          
        case x => LetP(name, prim, args, shrinkT(body)(s.withExp(name, prim, args)))
      }
      
      // If
      case If(cond, args, thenC, elseC) if args.forall(a => s.lEnv.contains(a)) 
            && cEvaluator.isDefinedAt((cond, args.map(a => s.lEnv.get(a).get))) => {
        val value = cEvaluator((cond, args.map(a => s.lEnv.get(a).get)))
        if (value) {
          AppC(thenC, Nil)
        }
        else {
          AppC(elseC, Nil)
        }
      }
      
      // Functions
      case LetF(functions, body) => {
        // DCE
        val fcts = functions.filterNot(f => s.dead(f.name))
        
        // η-reduction
        def isEtaReduceable(f: FunDef): Boolean = f.body match {
          case AppF(name, retC, args) => (args == f.args)
          case _ => false
        }
        
        val etaReduceMap = fcts.filter(isEtaReduceable(_)).map(f => (f.body: @unchecked) match {
          case AppF(name, retC, args) => (f.name, name)
        })
        val notEtaReduceableFcts = fcts.filterNot(isEtaReduceable(_))
        
        val etaReducedFcts = notEtaReduceableFcts.map(f => f.copy(body = f.body.subst(PartialFunction[Name,Name] {
          case x => etaReduceMap.find(e => e._1 == x) match {
            case Some(entry) => entry._2
            case None => x
          }
        }))).map(f => f.copy(args = f.args.map(arg => etaReduceMap.find(e => e._1 == arg) match {
            case Some(entry) => entry._2
            case None => arg
        })))
        
        val bodySubst = body.subst(PartialFunction[Name, Name] {
          case x => etaReduceMap.find(e => e._1 == x) match {
            case Some(entry) => entry._2
            case None => x
          }
        })
        
        etaReducedFcts match {
          case Nil => shrinkT(bodySubst)
          case x::xs => LetF(etaReducedFcts.map(f => 
              f.copy(body = shrinkT(f.body)(s.withEmptyInvEnvs))), 
            shrinkT(bodySubst))
        }
      }
      
      // Continuations
      case LetC(continuations, body) => {
        // DCE
        val cnts = continuations.filterNot(c => s.dead(c.name))
        
        // η-reduction
        def isEtaReduceable(c: CntDef): Boolean = c.body match {
          case AppC(name, args) => (args == c.args)
          case _ => false
        }
        
        val etaReduceMap = cnts.filter(isEtaReduceable(_)).map(c => (c.body: @unchecked) match {
          case AppC(name, args) => (c.name, name)
        })
        val notEtaReduceableCnts = cnts.filterNot(isEtaReduceable(_))
        
        val etaReducedCnts = notEtaReduceableCnts.map(c => c.copy(body = c.body.subst(PartialFunction[Name,Name] {
          case x => etaReduceMap.find(e => e._1 == x) match {
            case Some(entry) => entry._2
            case None => x
          }
        }))).map(c => c.copy(args = c.args.map(arg => etaReduceMap.find(e => e._1 == arg) match {
            case Some(entry) => entry._2
            case None => arg
        })))
        
        val bodySubst = body.subst(PartialFunction[Name, Name] {
          case x => etaReduceMap.find(e => e._1 == x) match {
            case Some(entry) => entry._2
            case None => x
          }
        })
        
        etaReducedCnts match {
          case Nil => shrinkT(bodySubst)
          case x::xs => LetC(etaReducedCnts.map(x => 
            CntDef(x.name, x.args, shrinkT(x.body))), 
            shrinkT(bodySubst))
        }
      }
      
      case x =>
        x
    }
    
    val t = shrinkT(tree)(State(census(tree)))
    //printTree("Tree shrinked :", t)
    t
  }

  // (Non-shrinking) inlining:
  private def inline(tree: Tree, maxSize: Int): Tree = {

    val fibonacci = Seq(1, 2, 3, 5, 8, 13)

    val trees = Stream.iterate((0, tree), fibonacci.length) { case (i, tree) =>
      val funLimit = fibonacci(i)
      val cntLimit = 2*i
      
      def cntDefFreshNames(c: CntDef): Map[Name, Name] = freshNames(c.body, (
            (Symbol.fresh("inlC"), c.name) ::
            c.args.map(a => (Symbol.fresh("inlCA"), a))
        ).foldLeft(Map[Name,Name]())(_ + _))
      
      def funDefFreshNames(f: FunDef): Map[Name, Name] = freshNames(f.body, (
            (Symbol.fresh("inlF"), f.name) ::
            (Symbol.fresh("inlFretC"), f.retC) ::
            f.args.map(a => (Symbol.fresh("inlFA"), a))
        ).foldLeft(Map[Name,Name]())(_ + _))
      
      def freshNames(t: Tree, subst: Map[Name, Name]) : Map[Name, Name] = t match {
        case LetL(name, value, body) => freshNames(body, subst + (Symbol.fresh("inlL") -> name))
        case LetP(name, prim, args, body) => freshNames(body, subst + (Symbol.fresh("inlP") -> name))
        case LetC(continuations, body) => {
          val freshMap = continuations.foldLeft(Map[Name,Name]())(
              (m, c) => m ++ cntDefFreshNames(c))
          freshNames(body, freshMap)
        }
        case LetF(functions, body) => {
          val freshMap = functions.foldLeft(Map[Name,Name]())(
              (m, f) => m ++ funDefFreshNames(f))
              
          freshNames(body, freshMap)
        }
        case _ => Map[Name,Name]()
      }
      
      //printTree("Tree to inline :", tree)
      
      def inlineT(tree: Tree)(implicit s: State): Tree = tree match {
        case LetL(name, value, body) =>
          LetL(name, value, inlineT(body))
        case LetP(name, prim, args, body) =>
          LetP(name, prim, args, inlineT(body))
        
        case LetC(continuations, body) if size(tree) <= cntLimit => {
          def isInlineable(c: CntDef): Boolean = 
            s.appliedOnce(c.name) && continuations.forall(c2 => {
              census(c2.body).getOrElse(c.name, 0) == 0
            })
          
          val continuationsToInline = continuations.filter(isInlineable(_))
          
          val state = s.withCnts(continuationsToInline)

          LetC(continuations.filterNot(isInlineable(_)).map(c => 
            c.copy(body = inlineT(c.body)(state))), 
            inlineT(body)(state))
        }
        
        case LetC(continuations, body) =>
          LetC(continuations.map(c => c.copy(body = inlineT(c.body))), inlineT(body))
        
        case AppC(name, args) if s.cEnv.contains(name) => {
          val cntDef = s.cEnv.get(name).get
          val freshNamesMap = freshNames(cntDef.body, Map[Name, Name]())
          
          val freshBody = cntDef.body.subst(PartialFunction[Name, Name] {
            case x => freshNamesMap.get(x) match {
              case Some(n) => n
              case None => x
            }
          })
          
          inlineT(freshBody.subst(PartialFunction[Name, Name] {
            case x if cntDef.args.contains(x) => args(cntDef.args.indexOf(x))
            case x => x
          }))
        }
        case LetF(functions, body) if size(tree) < funLimit => {
          def isInlineable(f: FunDef): Boolean = 
            s.appliedOnce(f.name) && functions.forall(f2 => {
              census(f2.body).getOrElse(f.name, 0) == 0
            })
            
          val functionsToInline = functions.filter(isInlineable(_))
          
          LetF(functions.filterNot(isInlineable(_)).map(f => 
            f.copy(body = inlineT(f.body)(s.withFuns(functionsToInline)))),
            inlineT(body)(s.withFuns(functionsToInline)))
        }
        
        case LetF(functions, body) =>
          LetF(functions.map(f => f.copy(body = inlineT(f.body))), inlineT(body))
          
        
        case AppF(name, retC, args) if s.fEnv.contains(name) => {
          val funDef = s.fEnv.get(name).get
          val freshNamesMap = freshNames(funDef.body, Map[Name, Name]())
          
          val freshBody = funDef.body.subst(PartialFunction[Name, Name] {
            case x => freshNamesMap.get(x) match {
              case Some(n) => n
              case None => x
            }
          })
          
          inlineT(freshBody.subst(PartialFunction[Name, Name] {
            case x if x == funDef.retC => retC
            case x if funDef.args.contains(x) => args(funDef.args.indexOf(x))
            case x => x
          }))
        }
        
        case _ =>
          tree
      }
      
      val ret = inlineT(tree)(State(census(tree)))
      //printTree("Inlined tree :", ret)
      (i + 1, ret)
    }

    val tr = (trees takeWhile { case (_, tree) => size(tree) <= maxSize }).last._2
    //printTree("Inlined tree :", tr)
    tr
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
  
  protected def isBlockGet(v: ValuePrimitive): Boolean
  protected def isBlockLength(v: ValuePrimitive): Boolean
  protected def isBlockTag(v: ValuePrimitive): Boolean
  
  protected def getBlockTag(v: ValuePrimitive): Literal
  
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
  
  protected def isBlockGet(v: ValuePrimitive): Boolean = (v == L3BlockGet)
  protected def isBlockLength(v: ValuePrimitive): Boolean = (v == L3BlockLength)
  protected def isBlockTag(v: ValuePrimitive): Boolean = (v == L3BlockTag)
  
  protected def getBlockTag(v: ValuePrimitive): Literal = v match {
    case L3BlockAlloc(x) => IntLit(x)
    case _ => ???
  }
  
  protected def isFixedBlockAlloc(prim: ValuePrimitive): Boolean = prim match {
    case L3BlockAlloc(x) if x == 202 => true
    case _ => false
  }
  
  protected def hasBorderEffect(prim: ValuePrimitive): Boolean = prim match {
    case L3IntDiv | L3IntMod => true
    case _ => false
  }

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] =
    Set((IntLit(0), L3IntAdd), (IntLit(0), L3IntBitwiseOr), (IntLit(0), L3IntBitwiseXOr),
        (IntLit(1), L3IntMul))
  protected val rightNeutral: Set[(ValuePrimitive, Literal)] =
    Set((L3IntAdd, IntLit(0)), (L3IntSub, IntLit(0)), (L3IntBitwiseOr, IntLit(0)), (L3IntBitwiseXOr, IntLit(0)),
        (L3IntMul, IntLit(1)), (L3IntDiv, IntLit(1)),
        (L3IntArithShiftLeft, IntLit(0)), (L3IntArithShiftRight, IntLit(0)))
  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] =
    Set((IntLit(0), L3IntMul), (IntLit(0), L3IntBitwiseAnd))
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] =
    Set((L3IntMul, IntLit(0)), (L3IntBitwiseAnd, IntLit(0)))
  protected val sameArgReduce: Map[ValuePrimitive, Literal] =
    Map(L3IntSub -> IntLit(0), L3IntDiv -> IntLit(1), L3IntMod -> IntLit(0), L3IntBitwiseXOr -> IntLit(0))

  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal] = {
    case (L3IntAdd, Seq(IntLit(x), IntLit(y))) => IntLit(x + y)
    case (L3IntSub, Seq(IntLit(x), IntLit(y))) => IntLit(x - y)
    case (L3IntMul, Seq(IntLit(x), IntLit(y))) => IntLit(x * y)
    case (L3IntDiv, Seq(IntLit(x), IntLit(y))) if (y != 0) => IntLit(x / y)
    case (L3IntMod, Seq(IntLit(x), IntLit(y))) if (y != 0) => IntLit(x % y)
    
    case (L3IntArithShiftLeft, Seq(IntLit(x), IntLit(y))) => IntLit(x << y)
    case (L3IntArithShiftRight, Seq(IntLit(x), IntLit(y))) => IntLit(x >> y)
    case (L3IntBitwiseAnd, Seq(IntLit(x), IntLit(y))) => IntLit(x & y)
    case (L3IntBitwiseOr, Seq(IntLit(x), IntLit(y))) => IntLit(x | y)
    case (L3IntBitwiseXOr, Seq(IntLit(x), IntLit(y))) => IntLit(x ^ y)
  }

  protected val cEvaluator: PartialFunction[(TestPrimitive, List[Literal]),
                                            Boolean] = {
    case (L3IntP, Seq(IntLit(_))) => true
    case (L3CharP, Seq(CharLit(_))) => true
    case (L3BoolP, Seq(BooleanLit(_))) => true
    case (L3UnitP, Seq(UnitLit)) => true
    case (L3IntLt, Seq(IntLit(x), IntLit(y))) => x < y
    case (L3IntLe, Seq(IntLit(x), IntLit(y))) => x <= y
    case (L3Eq, Seq(IntLit(x), IntLit(y))) => x == y
    case (L3Ne, Seq(IntLit(x), IntLit(y))) => x != y
    case (L3IntGe, Seq(IntLit(x), IntLit(y))) => x >= y
    case (L3IntGt, Seq(IntLit(x), IntLit(y))) => x > y
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
  
  protected def isBlockGet(v: ValuePrimitive): Boolean = (v == CPSBlockGet)
  protected def isBlockLength(v: ValuePrimitive): Boolean = (v == CPSBlockSize)
  protected def isBlockTag(v: ValuePrimitive): Boolean = (v == CPSBlockTag)
  
  protected def getBlockTag(v: ValuePrimitive): Literal = v match {
    case CPSBlockAlloc(x) => x
    case _ => ???
  }
  
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
