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
}

object RecursionPracticeMain extends App {
  val list: List[Int] = 5 :: 2 :: 9 :: 10 :: 2 :: 4 :: 0 :: 10 :: 8 :: 17 :: Nil

  println(RecursionPractice.sum(3))

  println(RecursionPractice.min(list, 9))

  println(RecursionPractice.sumOfElements(list, 9))

  val charList: List[Char] = 'a' :: 'b' :: 'c' :: 'b' :: 'a' :: Nil
  println(RecursionPractice.isPalindrome(charList, 3))
}

