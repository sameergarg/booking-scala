import Domain.Event.{RoomAdded, RoomFetched}
import Domain._

class BookingService {

  // adds room to booking, stores an event
  def addRoom(booking: Booking)(
               no: String,
               floor: Int,
               view: Boolean,
               capacity: Int
             ): (Booking, Room) = {
    val newRoom = RoomGenerator.generateRoom(no, floor, view, capacity)
    (
      booking.copy(rooms = newRoom :: booking.rooms, events = RoomAdded(newRoom.no):: booking.events),
      newRoom
    )
  }

  // returns current reservation id
  def currentReservationId(booking: Booking): ReservationId = booking.rooms.flatMap(_.booked).map(_.id).foldLeft(0)(math.max(_, _))

  // fetches room by number
  def fetchRoom(booking: Booking)(no: String): (Booking, Option[Room]) = (
    booking.copy(events = RoomFetched(no)::booking.events),
    booking.rooms.filter(_.no == no).headOption
  )

  // books a guest to a room for a given period
  def book(booking: Booking)(
            room: Room,
            period: Period,
            guest: Guest,
            reservationId: ReservationId): Booking = ???

  // book vor VIP = book given room, if it does not exist then build it
  def bookVip(
               no: String,
               floor: Int,
               view: Boolean,
               capacity: Int,
               period: Period
             )(guest: Guest): Booking = ???

}

object RoomGenerator {
  def generateRoom(
                    no: String,
                    floor: Int,
                    view: Boolean,
                    capacity: Int
                  ): Room =
    Room(no, floor, view, capacity, capacity * 100, 10.0, booked = List.empty[Reservation])
}
