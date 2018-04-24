import scala.math.Ordering


class Sorter {
  def sort[A: Ordering](as: List[A]): List[A] = {
    as.sorted.reverse
  }
}

object Sorter {
  implicit val roomsSorter = new Sorter
}


