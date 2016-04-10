package linkedList

import scala.annotation.tailrec

/**
  * Created by brianmomongan on 4/8/16.
  */
case class Node[T](value: T, var next: Node[T] = null)

class LinkedListImpl[T] {

  private var head: Node[T] = null
  private var tail: Node[T] = null
  private var listCount: Int = 0


  private def first(element: Node[T]) = {
    if (head == null) {
      head = element
      tail = element
    }
  }

  def append(value: T) = {
    listCount += 1
    val element = Node[T](value)

    if (head == null) first(element)
    else {
      tail.next = element
      tail = element
    }
  }

  def push(value: T) = {
    listCount += 1
    val element = Node[T](value)
    if (head == null) first(element)
    else {
      element.next = head
      head = element
    }
  }

  def delete() = {
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

  def pop() = {
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

  def count(value: T): Int = {

    @tailrec
    def loop(current: Node[T], acc: Int = 0): Int = {
      if (current != null) {
        if (current.value == value) loop(current.next, acc + 1) else loop(current.next, acc)
      }
      else acc
    }
    loop(head)
  }

  def get(value: Int): Int = {
    if (head != null) {

      @tailrec
      def loop(current: Node[T], index: Int = 0): Int = {
        if (current == null) -1
        else if (current.value == value) index else loop(current.next, index + 1)
      }
      loop(head)
    }
    else -1
  }

  def insert(value: T, index: Int): Boolean = {
    val element = Node(value)
    if (head == null) {
      first(element)
      true
    }

    else if (index == 0) {
      element.next = head
      head = element
      true
    }

    else if (index == listCount) {
      tail.next = element
      tail = element
      true
    }

    else if (index > 0   && index < listCount  ) {

      @tailrec
      def loop(current: Node[T], cIndex: Int = 0): Unit = {
        if (index - 1 == cIndex) {
          element.next = current.next
          current.next = element
        } else loop(current.next, cIndex + 1)
      }
      loop(head)
      true
    }

    else false
  }

}

object Main extends App {

  val lk = new LinkedListImpl[Int]
  lk append 3
  lk append 4
  lk append 51
  lk append 21
  println("elements added to tail")
  lk.traverse foreach (element => println(element.value))
  println("length: " + lk.length)
  println()

  println("elements deleted from tail")
  lk.delete()
  lk.delete()
  lk.traverse foreach (element => println(element.value))
  println("length: " + lk.length)
  println()

  println("elements added to head")
  lk push 200
  lk.traverse foreach (element => println(element.value))
  println("length: " + lk.length)
  println()

  println("elements deleted from head")
  lk pop()
  lk.traverse foreach (element => println(element.value))
  println("length: " + lk.length)
  println()

  println("test count function")
  lk append 1
  println("count: " + lk.count(2))
  lk.traverse foreach (element => println(element.value))
  println()

  println("test get function")
  println("get: " + lk.get(1))
  lk push 1
  println("get: " + lk.get(10))
  println()

  println("test insert function")
  println(lk insert(100,-1))
  lk.traverse foreach (element => println(element.value))
}