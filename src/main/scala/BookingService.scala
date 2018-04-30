import Domain.Event.{ReservationMade, RoomAdded, RoomFetched}
import Domain._
import cats.data.State

class BookingService {

  type BookingState[A] = State[Booking, A]

  // adds room to booking, stores an event
  def addRoom(
    no: String,
    floor: Int,
    view: Boolean,
    capacity: Int
  ): Booking => (Booking, Room) = booking => {
    val newRoom = RoomGenerator.generateRoom(no, floor, view, capacity)
    val newBooking = booking.copy(rooms = newRoom :: booking.rooms, events = RoomAdded(newRoom.no) :: booking.events)
    (newBooking, newRoom)
  }

  // returns current reservation id
  private def currentReservationId: Booking => (Booking, ReservationId) =
    booking => (booking, booking.rooms.flatMap(_.booked).map(_.id).foldLeft(0)(math.max(_, _)))

  // fetches room by number
  def fetchRoom(no: String): Booking => (Booking, Option[Room]) = booking => (
    booking.copy(events = RoomFetched(no) :: booking.events),
    booking.rooms.filter(_.no == no).headOption
  )

  // books a guest to a room for a given period
  def book(
    room: Room,
    period: Period,
    guest: Guest,
    reservationId: ReservationId):Booking => (Booking, Unit) = booking => {
    val roomReserved = room.copy(booked = Reservation(reservationId, period, guest) :: room.booked)
    val otherRooms = booking.rooms.filter(_ != room)
    val newBooking = booking.copy(
      rooms = roomReserved :: otherRooms,
      events = ReservationMade(reservationId) :: booking.events
    )
    (newBooking, ())
  }

  // book vor VIP = book given room, if it does not exist then build it
  def bookVip(
    no: String,
    floor: Int,
    view: Boolean,
    capacity: Int,
    period: Period
  )(guest: Guest): Booking => (Booking, ReservationId) = booking => {
    val (fetchedRoomBooking, mayBeRoom: Option[Room]) = fetchRoom(no)(booking)
    val (addedRoomBooking, newRoom) = mayBeRoom.fold {
      addRoom(no, floor, view, capacity)(fetchedRoomBooking)
    } {
      room => (fetchedRoomBooking: Booking, room)
    }
    val resId = currentReservationId(addedRoomBooking)._2 + 1
    val (newBooking, _) = book(newRoom, period, guest, resId)(addedRoomBooking)
    (newBooking, resId)
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

