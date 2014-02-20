package l3

/**
 * Helper module for IO functions in L₃ and intermediate languages.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object IO {
  def readChar(): Char =
    System.in.read().toChar

  def printChar(c: Char): Unit = {
    System.out.print(c)
    System.out.flush()
  }
}
