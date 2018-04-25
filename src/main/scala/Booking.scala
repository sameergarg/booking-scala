import java.time.LocalDate

import Domain._
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

  val pickAvailable: (Period, List[Room]) => List[Room] = (period, rooms) => rooms.filter(!_.booked.map(_.period).contains(period))
  val filterWithView: List[Room] => List[Room] = _.filter(_.view)
  val canAccommodate: (NoPpl, List[Room]) => List[Room] = (noPpl, rooms) => rooms.filter(_.capacity >= noPpl)

  val sortByRating: List[Room] => List[Room] = _.sorted

  val costPerPerson: Room => Double = room => room.price / room.capacity

  // available & with View & has best rating
  val proposeBest: (Booking, Period, NoPpl) => Option[Room] = { (booking, period, noPpl) =>

    val roomWithCapacity: List[Room] => List[Room] = canAccommodate.curried(noPpl)
    val roomNotBooked: List[Room] => List[Room] = pickAvailable.curried(period)

    val best: List[Room] => Option[Room] = roomWithCapacity >>> roomNotBooked >>> filterWithView >>> sortByRating >>> (rooms => rooms.headOption)

    best(booking.rooms)
  }

  val costPerPersonForBest: (Booking, Period, NoPpl) => Option[Double] = Function.untupled(proposeBest.tupled >>> (mayBeRoom => mayBeRoom.map(costPerPerson)))
}

