package sorting.bubbleSort

import scala.annotation.tailrec

/**
  * Created by brianmomongan on 4/11/16.
  */

class BubbleSortImpl[T <: Int] {

  def sortList(list: List[T]): List[T] = {

    val listLen = list.length

    def swap(list: List[T], from: Int): List[T] = {
      val swapped = list(from + 1) :: list(from) :: Nil
      val first = if (from == 0) List() else list.take(from)
      val last = if (from + 1 == list.length - 1) List() else list.drop(from + 2)
      first ++ swapped ++ last
    }

    @tailrec
    def loopToSort(list: List[T], current: Int = 0, it: Int): List[T] = {
      if (current < it) {
        if (list(current) > list(current + 1)) loopToSort(swap(list, current), current + 1, it)
        else loopToSort(list, current + 1, it)
      }
      else list
    }

    @tailrec
    def loop(list: List[T], it: Int = listLen): List[T] = {
      if (0 < it) loop(loopToSort(list, 0, it - 1), it - 1)
      else list
    }
    loop(list)
  }
}

object BubbleSortMain extends App {

  val bubble = new BubbleSortImpl[Int]
  val list: List[Int] = 5 :: 2 :: 0 :: 9 :: 1 :: 2 :: 4 :: Nil
  println(bubble.sortList(list))
}

