package sorting.quicksort

import scala.annotation.tailrec

/**
  * Created by brianmomongan on 4/13/16.
  */
class QuickSortImpl[T <: Int] {


  def sort(list: List[T]): List[T] = {
    val len = list.length - 1

    def swap(list: List[T], index: Int, right: Int): List[T] = {
      //      println(list)
      val mutate = scala.collection.mutable.ArraySeq(list: _*)
      val swapped = mutate(right)
      mutate(right) = mutate(index)
      mutate(index) = swapped
      mutate.toList
    }

    def loop(list: List[T], index: Int = 0, left: Int = 0, right: Int = len, end: Int = len): List[T] = {
      if (left <= right) {
        if (list(left) <= list(index)) loop(list, index, left + 1, right, end)
        else if (list(right) >= list(index)) loop(list, index, left, right - 1, end)
        else loop(swap(list, left, right), index, left + 1, right - 1, end)
      }
      else recursion(swap(list, index, right), right, end)
    }

    def recursion(list: List[T], index: Int = len, end: Int = len): List[T] = {
      println(list)
      if (index == 0) list
      else {
        loop(list, 0, 0, index - 1, index - 1)
        loop(list, index + 1, index + 1, end, end)
      }
    }
    loop(list)
  }

}

object QuickSortMain extends App {

  val list: List[Int] = 5 :: 2 :: 0 :: 9 :: 9 :: 1 :: 4 :: 2 :: 7 :: Nil

  println(list)
  val quick = new QuickSortImpl[Int]
  println(quick.sort(list))
}
