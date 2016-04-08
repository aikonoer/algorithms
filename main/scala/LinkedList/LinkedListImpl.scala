package LinkedList

import scala.annotation.tailrec

/**
  * Created by brianmomongan on 4/8/16.
  */
case class Node[T](value: T, var next: Node[T] = null)

class LinkedListImpl[T] {

  var head: Node[T] = null
  var tail: Node[T] = null
  var listCount: Int = 0

  def addTail(value: T) = {
    listCount += 1
    val element = Node[T](value)
    if (head == null) {
      head = element
      tail = element
    }
    else {
      tail.next = element
      tail = element
    }
  }

  def addHead(value: T) = {
    listCount += 1
    val element = Node[T](value)
    if (head == null) {
      head = element
      tail = element
    }
    else {
      element.next = head
      head = element
    }
  }

  def deleteTail() = {
    if (head != null) {

      @tailrec
      def loop(current: Node[T]): Unit = {
        if (current.next == null) {
          head = null
          tail = null
        }
        else if (current.next.next != null) loop(current.next)
        else {
          current.next = null
          tail = current
        }
      }
      loop(head)
      listCount -= 1
    }
  }

  def deleteHead() = {
    if (head != null) {
      head = head.next
      listCount -= 1
    }
  }

  def length: Int = listCount

  def traverse: List[Node[T]] = {

    @tailrec
    def loop(node: Node[T], list: List[Node[T]] = List[Node[T]]()): List[Node[T]] = {
      if (node.next != null) loop(node.next, node :: list) else node :: list
    }
    loop(head).reverse
  }

}

object Main extends App {

  val lk = new LinkedListImpl[Int]
  lk addTail 3
  lk addTail 4
  lk addTail 51
  lk addTail 21
  println("elements added to tail")
  lk.traverse foreach (element => println(element.value))
  println("length: " + lk.length)
  println()

  println("elements deleted from tail")
  lk.deleteTail()
  lk.deleteTail()
  lk.traverse foreach (element => println(element.value))
  println("length: " + lk.length)
  println()

  println("elements added to head")
  lk addHead 200
  lk.traverse foreach (element => println(element.value))
  println("length: " + lk.length)
  println()

  println("elements deleted from head")
  lk deleteHead()
  lk.traverse foreach (element => println(element.value))
  println("length: " + lk.length)
}