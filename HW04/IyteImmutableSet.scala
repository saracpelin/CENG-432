class IyteImmutableSet {

  private var root: Node = null;
  var result: String = "";

  def this(root: Node) {
    this();
    this.root = root;
  }

  def add(x: Int): IyteImmutableSet = {
    new IyteImmutableSet(add(x, root));
  }

  private def add(x: Int, node: Node): Node = {
    var newNode: Node = null;
    if (node == null) {
      newNode = new Node(x);
    } else if (x < node.getData()) {
      newNode = new Node(node.getData());
      newNode.setRight(node.getRight());
      newNode.setLeft(add(x, node.getLeft))
      if (findHeight(newNode.getLeft) - findHeight(newNode.getRight) > 1) {
        if (x < newNode.getLeft.getData) {
          newNode = rotateLeft(newNode);
        } else {
          newNode = doubleRotateLeft(newNode);
        }
      }
    } else if (x > node.getData) {
      newNode = new Node(node.getData());
      newNode.setLeft(node.getLeft);
      newNode.setRight(add(x, node.getRight));
      if (findHeight(newNode.getRight) - findHeight(newNode.getLeft) > 1) {
        if (x > newNode.getRight.getData) {
          newNode = rotateRight(newNode);
        } else {
          newNode = doubleRotateRight(newNode);
        }
      }
    } else {
      println(node.getData() +"Set contains it")
    }
    newNode.setHeight(math.max(findHeight(newNode.getLeft), findHeight(newNode.getRight)) + 1);
    newNode;
  }

  def contains(x: Int): Boolean = {
    contains(x, this.root);
  }

  private def contains(x: Int, node: Node): Boolean = {
    var n = node;
    var result = false;
    if (n != null && !result) {
      var temp = n.getData;
      if (x < temp) {
        n = n.getLeft;
      } else if (x > temp) {
        n = n.getRight;
      } else {
        result = true;
      }
      if (!result) {
        result = contains(x, n);
      }
    }
    result;
  }

  override def toString(): String = {
    result = "";
    makeString(root);
    if (result.size > 0) {
      result = result.substring(0, result.size - 1)
    } else {
      result = "Empty Set!";
    }
    result;
  }

  private def makeString(node: Node): String = {
    if (node != null) {
      makeString(node.getLeft);
      result = result + node.getData() + ",";
      makeString(node.getRight);
    }
    result;
  }

  private def findHeight(n: Node): Int = {
    if (n == null) {
      -1
    } else {
      n.getHeight();
    }
  }

  private def rotateLeft(node: Node): Node = {
    var temp = node.getLeft;
    node.setLeft(temp.getRight);
    temp.setRight(node);
    node.setHeight(math.max(findHeight(node.getLeft), findHeight(temp.getRight)) + 1);
    temp.setHeight(math.max(findHeight(temp.getLeft), node.getHeight) + 1);
    temp;
  }

  private def rotateRight(node: Node): Node = {
    var temp = node.getRight;
    node.setRight(temp.getLeft);
    temp.setLeft(node);
    node.setHeight(math.max(findHeight(node.getLeft), findHeight(temp.getRight)) + 1);
    temp.setHeight(math.max(findHeight(temp.getRight), node.getHeight) + 1);
    temp;
  }

  private def doubleRotateLeft(node: Node): Node = {
    node.setLeft(rotateRight(node.getLeft));
    rotateLeft(node);
  }

  private def doubleRotateRight(node: Node): Node = {
    node.setRight(rotateRight(node.getRight));
    rotateRight(node);
  }

}

object IyteImmutableSet {
  def apply() = new IyteImmutableSet();
}
