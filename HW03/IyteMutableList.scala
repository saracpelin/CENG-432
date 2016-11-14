
class IyteMutableList {

  private case class Node(value: Int, var next: Node)
  private var first: Node = null
  private var last: Node = first


  def add(x: Int): Unit = {
    first match {
      case null => {
        first = Node(x, null)
        last = first
      }
      case _ => {
        last.next = Node(x, null)
        last = last.next
      }
    }
  }

  override def toString: String = {
    var str = ""
    while(first.next != null) {
      str += first.value

      if(first.next != null) {
        str += ","
      }
      first = first.next
    }
    str += first.value
    str
  }

}

object IyteMutableList {
  def apply: IyteMutableList = new IyteMutableList()
}
