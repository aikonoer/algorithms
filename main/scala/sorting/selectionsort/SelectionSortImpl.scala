package sorting.selectionsort

import scala.annotation.tailrec

/**
  * Created by brianmomongan on 4/12/16.
  */
class SelectionSortImpl[T <: Int] {


  def sort(list: List[T]): List[T] = {

    val len = list.length

    def swap(list: List[T], end: Int, largest: Int): List[T] = {
      val mutate = scala.collection.mutable.ArraySeq(list: _*)
      val swapped = mutate(largest)
      mutate(largest) = mutate(end)
      mutate(end) = swapped
      mutate.toList
    }

    @tailrec
    def scan(list: List[T], end: Int, current: Int = 0, largest: Int = 0): List[T] = {
      if (current <= end) {
        if (list(current) > list(largest)) scan(list, end, current + 1, current)
        else scan(list, end, current + 1, largest)
      }
      else swap(list, end, largest)
    }

    @tailrec
    def loop(list: List[T], end: Int = len - 1): List[T] = {
      if (end >= 0) loop(scan(list, end), end - 1)
      else list
    }
    loop(list)
  }
}

object SelectionSortMain extends App {

  val list: List[Int] = 5 :: 2 :: 0 :: 9 :: 1 :: 2 :: 4 :: Nil
  val selection = new SelectionSortImpl[Int]
  println(selection.sort(list))

}
