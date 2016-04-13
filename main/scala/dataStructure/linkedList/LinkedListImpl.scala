package dataStructure.linkedList

import scala.annotation.tailrec

/**
  * Created by brianmomongan on 4/8/16.
  */
case class Node[T](value: T, var next: Option[Node[T]] = None)

class LinkedListImpl[T] {

  private var head: Option[Node[T]] = None
  private var tail: Option[Node[T]] = None
  private var listCount: Int = 0

  private def first(element: Node[T]) = {
    if (head.isEmpty) {
      head = Some(element)
      tail = Some(element)
    }
  }

  def add(value: T) = {
    listCount += 1
    val element = Node[T](value)

    if (head.isEmpty) first(element)
    else {
      tail.get.next = Some(element)
      tail = Some(element)
    }
  }

  def push(value: T) = {
    listCount += 1
    val element = Node[T](value)

    if (head.isEmpty) first(element)
    else {
      element.next = head
      head = Some(element)
    }
  }

  def delete() = {
    if (head.nonEmpty) {

      @tailrec
      def loop(current: Option[Node[T]]): Unit = {
        if (current.get.next.isEmpty) {
          head = None
          tail = None
        }
        else if (current.get.next.get.next.nonEmpty) loop(current.get.next)
        else {
          current.get.next = None
          tail = current
        }
      }
      loop(head)
      listCount -= 1
    }
  }

  def pop() = {
    if (head.nonEmpty) {
      head = head.get.next
      listCount -= 1
    }
  }

  def length: Int = listCount

  def traverse: Option[List[Node[T]]] = {

    @tailrec
    def loop(node: Option[Node[T]], list: List[Node[T]] = List[Node[T]]()): Option[List[Node[T]]] = node match {
      case Some(node) => node.next match {
        case Some(next) => loop(Some(next), node :: list)
        case None => Option(node :: list)
      }
      case None => None
    }

    loop(head)
  }

  def count(value: T): Int = {

    def loop(current: Option[Node[T]], acc: Int = 0): Int = current match {
      case Some(node) => node.value match {
        case data if data == value => loop(node.next, acc + 1)
        case _ => loop(node.next, acc)
      }
      case None => acc
    }

    loop(head)
  }

  /*
  * input : index
  * output: value
  * */
  def get(value: Int): Option[T] = value match {

    case index if index == listCount - 1 => tail match {
      case Some(node) => Some(node.value)
      case None => None
    }

    case index if 0 until listCount contains index => {

      @tailrec
      def loop(current: Option[Node[T]], ind: Int = 0): Option[T] = current match {
        case Some(node) if ind == index => Some(node.value)
        case Some(node) if ind != index => loop(node.next, ind + 1)
        case None => None
      }
      loop(head)
    }
    case _ => None
  }

  def insert(value: T, index: Int) = {

    lazy val element = Node(value)

    if (index == 0) push(value)
    else if (index == listCount) add(value)
    else if (0 until listCount contains index) {

      @tailrec
      def loop(current: Option[Node[T]], cIndex: Int = 0): Unit = cIndex match {
        case cInd if cInd == index - 1 => {
          element.next = current.get.next
          current.get.next = Some(element)
        }
        case _ => loop(current.get.next, cIndex + 1)
      }
      loop(head)
      listCount += 1
    }
  }
}

object LinkedListMain extends App {

  def printAll = {
    lk.traverse match {
      case Some(el) => el.reverse foreach (each => println(each.value))
      case None => "Nothing found"
    }
    println("length: " + lk.length)
    println()
  }

  val lk = new LinkedListImpl[Int]

  println("elements added to tail")
  lk add 3
  lk add 4
  lk add 51
  lk add 21
  printAll

  println("elements deleted from tail")
  lk.delete()
  lk.delete()
  printAll

  println("elements added to head")
  lk push 20
  lk push 21
  printAll

  println("elements deleted from head")
  lk pop()
  printAll

  println("test count function")
  lk add 1
  lk add 3
  printAll
  println("count: " + lk.count(3))

  printAll
  println("test get function")
  println("get: " + lk.get(5))
  println()

  println("test insert function")
  lk insert(100, 5)
  printAll

}