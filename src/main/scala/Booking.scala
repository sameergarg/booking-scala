import cats.implicits._

import scala.math.Ordering

case class Room(
                 no: String,
                 floor: Int,
                 view: Boolean,
                 capacity: Int,
                 price: Double,
                 rating: Double,
                 booked: Boolean)
object Room {
  implicit val roomsOrdering = new Ordering[Room] {
    override def compare(x: Room, y: Room): Int = x.rating compareTo y.rating
  }
}

case class Booking(rooms: List[Room])

object BookingSystem {

  val costPerPerson: Room => Double = room => room.price / room.capacity

  val pickAvailable: List[Room] => List[Room] = _.filter(!_.booked)
  val filterWithView: List[Room] => List[Room] = _.filter(_.view)
  val sortByRating: List[Room] => List[Room] = Sorter.roomsSorter.sort

  // available & with View & has best rating
  val proposeBest: Booking => Room = ((booking: Booking) => booking.rooms) >>>
                                      pickAvailable >>>
                                      filterWithView >>>
                                      sortByRating >>>
                                      (rooms => rooms.head)

}

