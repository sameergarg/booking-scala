import Domain.Event.RoomAdded
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
  def currentReservationId(booking: Booking): ReservationId = ???

  // fetches room by number
  def fetchRoom(booking: Booking)(no: String): Option[Room] = ???

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
