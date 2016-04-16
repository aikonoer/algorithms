package sorting.mergesort

/**
  * Created by brianmomongan on 4/15/16.
  */
object MergeSortImpl {


  def sort(list: List[Int]): List[Int] = {
    val len = list.length - 1

    def swap(list: List[Int], index: Int, value: Int): List[Int] = {
      val mutate = scala.collection.mutable.ArraySeq(list: _*)
      mutate(index) = value
      mutate.toList
    }

    def loop(list: List[Int], left: Int, mid: Int, right: Int, end: Int): List[Int] = {
      if (left > mid && right > end) Nil
      else if (left > mid) list(right) :: loop(list, left, mid, right + 1, end)
      else if (right > end) list(left) :: loop(list, left + 1, mid, right, end)
      else if (list(left) < list(right)) list(left) :: loop(list, left + 1, mid, right, end)
      else list(right) :: loop(list, left, mid, right + 1, end)
    }

    def partition(list: List[Int], start: Int = 0, end: Int = len): List[Int] = {
      if (start == end) list
      else {
        val mid = (end - start) / 2
        val firstLoop = partition(list, start, start + mid)
        val secondLoop = partition(firstLoop, start + mid + 1, end)
        val merge = loop(secondLoop, start, start + mid, start + mid + 1, end)
        secondLoop.take(start) ++ merge ++ secondLoop.drop(end + 1)
      }
    }
    partition(list)
  }


}

object MergeSortMain extends App {
  val random = scala.util.Random
  val list = (0 to 9).toList.map(l => random.nextInt(100))
  println(list)
  println(MergeSortImpl.sort(list))
}
