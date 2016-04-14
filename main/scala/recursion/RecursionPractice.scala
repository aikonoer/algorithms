package recursion

/**
  * Created by brianmomongan on 4/13/16.
  */


object RecursionPractice {
  //  Write a recursive function that computes the sum of all numbers from 1 to n, where n is given as parameter.

  def sum(n: Int): Int = {
    if (n <= 0) n
    else n + sum(n - 1)
  }

  //  Write a recursive function that finds and returns the minimum element in an array, where the array and its size are given as parameters.
  def min(list: List[Int], size: Int): Int = {
    if (size == 0) list(size)
    else if (list(size) < min(list, size - 1)) list(size)
    else min(list, size - 1)
  }

  //  Write a recursive function that computes and returns the sum of all elements in an array, where the array and its size are given as parameters.
  def sumOfElements(list: List[Int], size: Int): Int = {
    if (size == 0) list(size)
    else list(size) + sumOfElements(list, size - 1)
  }

  //  Write a recursive function that determines whether an array is a palindrome, where the array and its size are given as parameters.
  def isPalindrome(list: List[Char], size: Int): Boolean = {
    val len = list.length
    if (size <= (len - 1) / 2) true
    else if (list(size) == list(len - 1 - size)) isPalindrome(list, size - 1)
    else false
  }

  //  Write a recursive function that searches for a target in a sorted array using binary search,
  // where the array, its size and the target are given as parameters.

  //0,1,2,3,4,5,6,7,8,9 - 7
  //
  def binarySearch(list: List[Int], start: Int = 0, end: Int, target: Int): Int = {
    val half = end - start / 2

    if (target == list(half)) half
    else if (start == end) -1
    else if (target < list(half)) binarySearch(list, start, half - 1, target)
    else binarySearch(list, half + 1, end, target)
  }
}

object RecursionPracticeMain extends App {
  val list: List[Int] = 5 :: 2 :: 9 :: 10 :: 2 :: 4 :: 0 :: 10 :: 8 :: 17 :: Nil
  val charList: List[Char] = 'a' :: 'b' :: 'c' :: 'b' :: 'a' :: Nil
  val binList = List(2, 5, 8, 10, 14, 20, 21, 25)

  /*println(RecursionPractice.sum(3))

  println(RecursionPractice.min(list, 9))

  println(RecursionPractice.sumOfElements(list, 9))

  println(RecursionPractice.isPalindrome(charList, 3))*/

  println(RecursionPractice.binarySearch(binList, 0, 7, 10))
}
























