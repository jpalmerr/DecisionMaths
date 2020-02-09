def bubbleSortInt(inputList: List[Int]): List[Int] = {
  def sort(unordered: List[Int], result: List[Int]): List[Int] = {
    if (unordered.isEmpty) result
    else bubble(unordered, Nil, result)
  }


  def bubble(unordered: List[Int], tempList: List[Int], result: List[Int]): List[Int] = unordered match {
    case h1 :: h2 :: t =>
      if (h1 > h2) bubble(h1 :: t, h2 :: tempList, result)
      else bubble(h2 :: t, h1 :: tempList, result)
    case h1 :: t => sort(tempList, h1 :: result)
    case x => x
  }
  sort(inputList, Nil)
}

val unorderedList = List(1, 17, 14, 3, 2, 10, 8)

bubbleSortInt(unorderedList) // List[Int] = List(1, 2, 3, 8, 10, 14, 17)
