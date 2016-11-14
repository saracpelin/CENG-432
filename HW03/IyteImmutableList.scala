
trait IyteImmutableList {
  def add(x: Int): IyteImmutableList
}

case object Empty extends IyteImmutableList {
  override def add(x: Int): IyteImmutableList = Node(x, Empty)
  override def toString: String = ""
}

case class Node(head: Int, tail: IyteImmutableList) extends IyteImmutableList {
  override def add(x: Int): IyteImmutableList = Node(x, this)
  override def toString: String = {
    tail match {
      case Empty => head.toString
      case _ => head + "," + tail
    }
  }
}

object IyteImmutableList {
  def apply: IyteImmutableList = Empty
}
