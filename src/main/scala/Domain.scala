import java.time.LocalDate

import scala.math.Ordering

object Domain {

  type NoPpl = Int
  type ReservationId = Int
  type Price = Double

  sealed trait Event

  object Event {
    case class RoomFetched(no: String) extends Event
    case class RoomAdded(no: String) extends Event
    case class ReservationMade(id: ReservationId) extends Event
  }

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

  case class Booking(
                      rooms: List[Room] = List.empty[Room],
                      events: List[Event] = List.empty[Event]
                    )

}

