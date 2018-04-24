import java.time.LocalDate

import Domain.Room
import cats.implicits._

import scala.math.Ordering

object Domain {

  type NoPpl = Int
  type ReservationId = Int
  type Price = Double

  case class Period(from: LocalDate, to: LocalDate)
  case class Guest(firstName: String, lastName: String)
  case class Reservation(id: ReservationId, period: Period, guest: Guest)

  case class Room(
                   no: String,
                   floor: Int,
                   view: Boolean,
                   capacity: Int,
                   price: Price,
                   rating: Double,
                   booked: List[Reservation])

  object Room {
    implicit val roomsOrdering = new Ordering[Room] {
      override def compare(x: Room, y: Room): Int = y.rating compareTo x.rating
    }
  }


  case class Booking(rooms: List[Room] = List.empty[Room])
}



case class Booking(rooms: List[Room])

object BookingSystem {

  val costPerPerson: Room => Double = room => room.price / room.capacity

  val pickAvailable: List[Room] => List[Room] = _.filter(_.booked.isEmpty)
  val filterWithView: List[Room] => List[Room] = _.filter(_.view)
  val sortByRating: List[Room] => List[Room] = _.sorted

  // available & with View & has best rating
  val proposeBest: Booking => Room = ((booking: Booking) => booking.rooms) >>>
                                      pickAvailable >>>
                                      filterWithView >>>
                                      sortByRating >>>
                                      (rooms => rooms.head)

  val costPerPersonForBest: Booking => Double = proposeBest >>> costPerPerson
}

