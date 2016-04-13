package sorting.insertionSort

import scala.annotation.tailrec

/**
  * Created by brianmomongan on 4/12/16.
  */
class InsertionSortImpl[T <: Int] {


  def sort(list: List[T]): List[T] = {

    val len = list.length

    def swap(list: List[T], from: Int): List[T] = {
      val swapped = list(from) :: list(from - 1) :: Nil
      val first = if (from == 1) List() else list.take(from - 1)
      val last = if (from == len - 1) List() else list.drop(from + 1)
      first ++ swapped ++ last
    }

    @tailrec
    def insert(list: List[T], count: Int): List[T] = {
      if (count > 0 && list(count) < list(count - 1)) insert(swap(list, count), count - 1)
      else list
    }

    @tailrec
    def loop(list: List[T], count: Int = 0): List[T] = {
      if (count < len) loop(insert(list, count), count + 1)
      else list
    }

    loop(list)
  }

}


object InsertionSortMain extends App {

  //val list: List[Int] = 5 :: 2 :: 0 :: 9 :: 1 :: 2 :: 4 :: Nil
  val list = (1 to 10).toList
  val insert = new InsertionSortImpl[Int]
  println(insert.sort(list))

}
