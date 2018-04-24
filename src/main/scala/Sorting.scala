
object QuickSort {

  def sort(rooms: List[Room]): List[Room] = {
    rooms.sortBy(_.rating).reverse
  }

}

