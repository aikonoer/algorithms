val list: List[Int] = (1 to 10).toList

list match {
  case Nil => println("nothing")
  case x :: y :: last => println(tail)
  case _ => println("what's in here?")
}

