package l3.test.infrastructure

import scala.util.parsing.input.CharSequenceReader
import l3.L3Parser
import l3.L3Scanner
import l3.Formatter
import l3.input.SeqReader
import java.io._

/**
 * Common compiler testing infrastructure
 *
 * @author Vlad Ureche <vlad.ureche@epfl.ch>
 */
trait CompilerTest extends SandboxedTest {

  /** Compile the given source using a customizable set of phases given in pipeline */
  private def compileInner[T](source: () => String, pipeline: () => (l3.NominalCL3TreeModule.Tree => T)): T = {
    val inputReader = new CharSequenceReader(source())

    L3Parser.program(new L3Scanner.Scanner(inputReader)) match {
      case L3Parser.Success(program, _) =>
        pipeline()(program)
      case failure @ L3Parser.NoSuccess(_, _) =>
        assert(false, "\n" + failure); ???
    }
  }

  def compileUsingPipeline[T](source: () => String, pipeline: () => (l3.NominalCL3TreeModule.Tree => T)): T =
    sandboxedTest { compileInner(source, pipeline) }

  def compileUsingPipelineAndRedirect[T](source: () => String, pipeline: () => (l3.NominalCL3TreeModule.Tree => T), input: String): String =
    sandboxedTestWithRedirectedIO(compileInner(source, pipeline), input)

  def TreeToString[T](implicit f: Formatter[T]): TreeToString[T] = new TreeToString[T]

  /** A compiler phase that stores the tree as a string */
  class TreeToString[T](implicit f: Formatter[T]) extends Function1[T, String] {
    def apply(t: T): String = {
      val output = new java.io.StringWriter()
      f.toDocument(t).format(78, output)
      output.toString
    }
  }
}
