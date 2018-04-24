import scala.math.Ordering


class Sorter[A] {
  def sort(as: List[A])(implicit ord: Ordering[A]): List[A] = {
    as.sorted.reverse
  }
}

object Sorter {
  implicit val roomsSorter = new Sorter[Room]
}


