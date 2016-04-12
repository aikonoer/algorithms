val list: List[Int] = (1 to 10).toList

val s = scala.collection.mutable.ArraySeq(list:_*)

s(1) = 100

s.toList
