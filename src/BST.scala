import scala.collection.mutable

/**
  * Created by manojmohan on 4/18/16.
  * Based on mycodeschool's youtube lectures
  */
object BST {

  var root: Node = null

  class Node(var data:Int, var left: Node, var right: Node)

  def insert(data:Int) {
    def insert(root:Node, data: Int): Node = {
      var temp: Node = root
      if (temp == null) {
        temp = new Node(data, null, null)
      } else if (data <= root.data) {
        temp.left = insert(temp.left, data)
      } else {
        temp.right = insert(temp.right, data)
      }
      temp
    }

    root = insert(root, data)
  }

  def search(data:Int): Boolean = {
    def search(root: Node, data: Int): Boolean = {
      if (root == null) false
      else if (data == root.data) true
      else if (data < root.data) search(root.left, data)
      else search(root.right, data)
    }
    search(root, data)
  }

  private def findMin(root: Node): Int = {
    if (root.left == null) return root.data
    findMin(root.left)
  }

  def findMin() : Int = {
    findMin(root)
  }

  def findMax() : Int = {
    def findMax(root: Node): Int = {
      if (root.right == null) return root.data
      findMax(root.right)
    }
    findMax(root)
  }

  def findHeight() : Int = {
    def findHeight(root: Node): Int = {
      if (root == null) return -1
      return math.max(findHeight(root.left), findHeight(root.right)) + 1
    }
    findHeight(root)
  }

  def delete(data: Int) = {
    def delete(root: Node, data: Int) : Node = {
      if (root == null) return null
      var temp: Node = root
      if (root.data == data) {
        if (root.left == null && root.right == null) {  // no child
          temp = null
        } else if (root.right == null) {  // only left child
          temp = root.left
        } else if (root.left == null) {  // only right child
          temp = root.right
        } else {                         // has left and right child
          val min = findMin(root.right)  // find min
          temp.data = min
          temp.right = delete(root.right, min) // this reduces to one child node since min won't have left child
        }
      } else if (root.data > data) {
        temp.left = delete(root.left, data)
      } else if (root.data < data) {
        temp.right = delete(root.right, data)
      }
      return temp
    }
    root = delete(root, data)
  }

  def inOrderTraversal(): Unit = {
    def inOrderTraversal(root: Node): Unit = {
      if (root == null) return
      inOrderTraversal(root.left)
      print(s"${root.data}\t")
      inOrderTraversal(root.right)
    }
    inOrderTraversal(root)
    println()
  }

  def preOrderTraversal(): Unit = {
    def preOrderTraversal(root: Node): Unit = {
      if (root == null) return
      print(s"${root.data}\t")
      preOrderTraversal(root.left)
      preOrderTraversal(root.right)
    }
    preOrderTraversal(root)
    println()
  }

  def postOrderTraversal(): Unit = {
    def postOrderTraversal(root: Node): Unit = {
      if (root == null) return
      postOrderTraversal(root.left)
      postOrderTraversal(root.right)
      print(s"${root.data}\t")
    }
    postOrderTraversal(root)
    println()
  }

  def levelOrderTraversal(): Unit = {
    if (root == null) return
    val queue = new mutable.Queue[Node]()
    println()
    queue.enqueue(root)
    while(!queue.isEmpty) {
      val node = queue.dequeue()
      print(s"${node.data}\t")
      if (node.left != null) queue.enqueue(node.left)
      if (node.right != null) queue.enqueue(node.right)
    }
    println()
  }

  def findInOrderSuccessor(data:Int): Node = {

    def findNode(root: Node, data:Int): Node = {
      if (root == null) return null
      if (data == root.data)
        return root
      else if (data < root.data)
        findNode(root.left, data)
      else
        findNode(root.right, data)
    }

    def findInOrderSuccessor(root: Node, dataNode:Node): Node = {


      if (root == null || dataNode == null) return null

      // node has right subtree
      if (dataNode.right != null) {
        //leftmost node in right subtree
        var temp = dataNode.right
        while(temp.left != null) {
          temp = temp.left
        }
        return temp
      } else {
        // go to nearest ancestor for which given node would be in left subtree
        var ancestor = root
        var successor: Node = null
        while(dataNode != successor) {
          if (dataNode.data < ancestor.data) {
            successor = ancestor
            ancestor = ancestor.left
          } else {
            ancestor = ancestor.right
          }
        }
        return successor
      }

    }
    findInOrderSuccessor(root, findNode(root, data))

  }

  def main(args: Array[String]) {
    insert(15)
    insert(10)
    insert(20)
    insert(25)
    insert(8)
    inOrderTraversal()
    preOrderTraversal()
    postOrderTraversal()
    levelOrderTraversal()
    println(s"Searching for 8 - ${search(8)}")
    println(s"Searching for 30 - ${search(30)}")
    println(s"Min is - ${findMin}")
    println(s"Max is - ${findMax}")
    println(s"Height is - ${findHeight}")
    delete(8)
    println(s"Searching for 8 - ${search(8)}")
    println(s"Searching for 15 - ${search(15)}")
    delete(15)
    println(s"Searching for 15 - ${search(15)}")
    println(s"root is - ${root.data}")
    println(s"root's children are - ${root.left.data}, ${root.right.data}")
    inOrderTraversal()
    insert(8)
    insert(15)
    insert(12)
    insert(6)
    insert(11)
    insert(16)
    insert(17)
    insert(27)
    inOrderTraversal()
    println(s"inorder successor of 12 is ${findInOrderSuccessor(12).data}")
    println(s"inorder successor of 6 is ${findInOrderSuccessor(6).data}")
    println(s"inorder successor of 25 is ${findInOrderSuccessor(25).data}")
  }

}
