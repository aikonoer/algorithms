package sorting.quicksort

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
      if (index > end) list
      else if (left <= right) {
        if (list(left) <= list(index)) loop(list, index, left + 1, right, end)
        else if (list(right) >= list(index)) loop(list, index, left, right - 1, end)
        else loop(swap(list, left, right), index, left + 1, right - 1, end)
      }
      else recursion(swap(list, index, right), index, right, end)
    }

    def recursion(list: List[T], start: Int, mid: Int, end: Int): List[T] = {
      if (start == end) list
      else {
        loop(loop(list, start, start, mid - 1, mid - 1), mid + 1, mid + 1, end, end)
      }
    }
    loop(list)
  }

}

object QuickSortMain extends App {

  val list1: List[Int] = 5 :: 2 :: 0 :: 9 :: 9 :: 1 :: 4 :: 2 :: 7 :: Nil
  val list2 = (0 to 20).toList.reverse
  println(list1)
  val quick = new QuickSortImpl[Int]
  println(quick.sort(list1))

  println(list2)
  println(quick.sort(list2))
}
