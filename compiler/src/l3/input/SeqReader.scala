package l3.input

import scala.util.parsing.input.{Reader, Position, NoPosition}

sealed trait SeqReader[+T] extends Reader[T]

final class SeqConsReader[T](index: Int,
                             rHead: Reader[T],
                             rTail: SeqReader[T]) extends SeqReader[T] {
  override def first: T =
    if (rHead.atEnd) rTail.first else rHead.first
  override def rest: Reader[T] =
    if (rHead.atEnd) rTail.rest else new SeqConsReader(index, rHead.rest, rTail)
  override def pos: Position =
    if (rHead.atEnd) rTail.pos else new SeqPosition(index, rHead.pos)
  override def atEnd: Boolean =
    rHead.atEnd && rTail.atEnd
}

object SeqNilReader extends SeqReader[Nothing] {
  object EmptyReaderException extends RuntimeException("empty reader")
  override def first: Nothing = throw EmptyReaderException
  override def rest: Reader[Nothing] = this
  override def pos: Position = NoPosition
  override def atEnd: Boolean = true
}

class SeqPosition(private val index: Int, pos: Position) extends Position {
  def line: Int = pos.line
  def column: Int = pos.column
  def lineContents: String = pos.longString.split("\n")(0) // HACK
  override def <(that: Position) = that match {
    case thatSeqPos: SeqPosition =>
      index < thatSeqPos.index || (index == thatSeqPos.index && super.<(that))
    case NoPosition =>
      false
    case _ =>
      sys.error("comparing a SeqPosition with another kind of position")
  }
  override def toString: String = pos.toString
}

object SeqReader {
  def apply[T](rs: Reader[T]*): SeqReader[T] =
    (rs.zipWithIndex :\ (SeqNilReader : SeqReader[T])) { (hi, t) =>
      new SeqConsReader(hi._2, hi._1, t) }
}
