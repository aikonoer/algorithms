package sorting.quicksort

import scala.annotation.tailrec

/**
  * Created by brianmomongan on 4/13/16.
  */
class QuickSortImpl[T <: Int] {


  def sort(list: List[T]): List[T] = {
    val len = list.length - 1

    def swap(list: List[T], index: Int, right: Int): List[T] = {
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
      else recursion(swap(list, index, right), right)
    }

    def recursion(list: List[T], right: Int = len): List[T] = {
      if (right > 1) loop(list, 0, 0, right, right)
      //      else if (right < len - 2) loop(list, right + 1, right + 1)
      else list
    }

    recursion(list)
  }
}

object QuickSortMain extends App {

  val list: List[Int] = 5 :: 2 :: 0 :: 9 :: 1 :: 2 :: 4 :: 1 :: 10 :: 8 :: 17 :: 0 :: Nil
  //2:46

  println(list)
  val quick = new QuickSortImpl[Int]
  println(quick.sort(list))
}
