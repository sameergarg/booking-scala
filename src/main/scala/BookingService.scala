import Domain.Event.{ReservationMade, RoomAdded, RoomFetched}
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
      booking.copy(rooms = newRoom :: booking.rooms, events = RoomAdded(newRoom.no) :: booking.events),
      newRoom
    )
  }

  // returns current reservation id
  def currentReservationId(booking: Booking): ReservationId = booking.rooms.flatMap(_.booked).map(_.id).foldLeft(0)(math.max(_, _))

  // fetches room by number
  def fetchRoom(booking: Booking)(no: String): (Booking, Option[Room]) = (
    booking.copy(events = RoomFetched(no) :: booking.events),
    booking.rooms.filter(_.no == no).headOption
  )

  // books a guest to a room for a given period
  def book(booking: Booking)(
    room: Room,
    period: Period,
    guest: Guest,
    reservationId: ReservationId): Booking = {
    val roomReserved = room.copy(booked = Reservation(reservationId, period, guest) :: room.booked)
    val otherRooms = booking.rooms.filter(_ != room)
    booking.copy(
      rooms = roomReserved :: otherRooms,
      events = ReservationMade(reservationId) :: booking.events
    )
  }

  // book vor VIP = book given room, if it does not exist then build it
  def bookVip(booking: Booking)(
    no: String,
    floor: Int,
    view: Boolean,
    capacity: Int,
    period: Period
  )(guest: Guest): (Booking, ReservationId) = {
    val (fetchedRoomBooking, mayBeRoom: Option[Room]) = fetchRoom(booking)(no)
    val (addedRoomBooking, newRoom) = mayBeRoom.fold {
      addRoom(fetchedRoomBooking)(no, floor, view, capacity)
    } {
      room => (fetchedRoomBooking: Booking, room)
    }
    val resId = currentReservationId(addedRoomBooking) + 1
    (book(addedRoomBooking)(newRoom, period, guest, resId), resId)
  }

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
