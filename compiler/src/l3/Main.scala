package l3

import java.nio.file.Path
import l3.input.{ FileReader, SeqReader }
import scala.util.parsing.input.{ StreamReader, CharSequenceReader }
import CL3TreeFormatter._
import CPSTreeFormatter._

object Main extends MainHelper {

  def main(args: Array[String]): Unit = {
    try {
      val fs = java.nio.file.FileSystems.getDefault
      val inFiles = expandModules(fs.getPath(""), args.toList).distinct
      val inReader = SeqReader(inFiles map { f => FileReader(f.toString) }: _*)

      L3Parser.program(new L3Scanner.Scanner(inReader)) match {
        case L3Parser.Success(program, _) =>
          val backEnd = (
            CL3NameAnalyzer
            andThen treePrinter("Tree in CL3")
            andThen CL3ToCPSTranslator
            andThen treePrinter("Tree in CPS")
            andThen CPSDataRepresenter
            andThen CPSInterpreterLow)
          backEnd(program)
        case failure @ L3Parser.NoSuccess(_, _) =>
          Console.println(failure)
          sys.exit(1)
      }
    } catch {
      case e: java.io.FileNotFoundException =>
        Console.println("error: Source file not found: " + e.getMessage())
    }
  }
}

trait MainHelper {

  protected def treePrinter[T](msg: String)(implicit f: Formatter[T]): T => T =
    passThrough { tree =>
      val writer = new java.io.PrintWriter(System.out)
      println(msg)
      f.toDocument(tree).format(80, writer)
      writer.println()
      writer.flush()
    }

  protected def expandModules(base: Path, pathNames: Seq[String]): Seq[Path] = {
    def readModule(modulePath: Path): Seq[String] = {
      import java.io.{ BufferedReader, FileReader }
      val moduleReader = new BufferedReader(new FileReader(modulePath.toFile))
      val moduleContents = (
        Iterator.continually(moduleReader.readLine)
        takeWhile (_ != null)
        map (_.trim)
        filterNot { s => (s startsWith ";") || s.isEmpty }).toList
      moduleReader.close()
      moduleContents
    }

    val basePath = base.toAbsolutePath.normalize
    pathNames flatMap { pn =>
      val p = basePath.resolve(pn).normalize
      if (p.getFileName.toString endsWith ".ml3")
        expandModules(p.getParent, readModule(p))
      else
        Seq(p)
    }
  }

  protected def passThrough[T](f: T => Unit): T => T = { t: T => f(t); t }

  protected def seqPrinter[T](msg: String): Seq[T] => Seq[T] =
    passThrough { program =>
      println(msg)
      for (elem <- program)
        println(elem)
    }
}
