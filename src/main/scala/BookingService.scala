import Domain.Event.{ReservationMade, RoomAdded, RoomFetched}
import Domain._
import cats.data.State
import cats.implicits._

object BookingService {

  type BookingState[A] = State[Booking, A]

  // adds room to booking, stores an event
  def addRoom(
               no: String,
               floor: Int,
               view: Boolean,
               capacity: Int
             ): BookingState[Room] = State(booking => {
    val newRoom = RoomGenerator.generateRoom(no, floor, view, capacity)
    val newBooking = booking.copy(rooms = newRoom :: booking.rooms, events = RoomAdded(newRoom.no) :: booking.events)
    (newBooking, newRoom)
  })

  // returns current reservation id
  private def currentReservationId: BookingState[ReservationId] = State {
    booking => (booking, booking.rooms.flatMap(_.booked).map(_.id).foldLeft(0)(math.max(_, _)))
  }

  // fetches room by number
  def fetchRoom(no: String): BookingState[Option[Room]] = State { booking =>
    (booking.copy(events = RoomFetched(no) :: booking.events),
      booking.rooms.filter(_.no == no).headOption)
  }

  // books a guest to a room for a given period
  def book(
            room: Room,
            period: Period,
            guest: Guest,
            reservationId: ReservationId): BookingState[Unit] = State { booking => {
    val roomReserved = room.copy(booked = Reservation(reservationId, period, guest) :: room.booked)
    val otherRooms = booking.rooms.filter(_ != room)
    val newBooking = booking.copy(
      rooms = roomReserved :: otherRooms,
      events = ReservationMade(reservationId) :: booking.events
    )
    (newBooking, ())
  }
  }

  // book vor VIP = book given room, if it does not exist then build it
  def bookVip(
               no: String,
               floor: Int,
               view: Boolean,
               capacity: Int,
               period: Period
             )(guest: Guest): BookingState[ReservationId] = for {
    maybeRoom <- fetchRoom(no)
    room <- maybeRoom.fold(addRoom(no, floor, view, capacity))(_.pure[BookingState])
    currentResId <- currentReservationId
    resId = currentResId + 1
    _ <- book(room, period, guest, resId)
  } yield resId
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

