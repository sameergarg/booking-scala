import BookingAlgebra.BookingState
import Domain.Event.{ReservationMade, RoomAdded, RoomFetched}
import Domain._
import cats.Monad
import cats.data.StateT
import cats.implicits._

import scala.language.higherKinds

trait BookingAlgebra[F[_]] {


  def addRoom(
              no: String,
              floor: Int,
              view: Boolean,
              capacity: Int
            ): BookingState[F, Room]

  def fetchRoom(no: String): BookingState[F, Option[Room]]

  def currentReservationId(): BookingState[F, ReservationId]

  def book(
           room: Room,
           period: Period,
           guest: Guest,
           reservationId: ReservationId): BookingState[F, Unit]

}

object BookingAlgebra {

  type BookingState[F[_], A] = StateT[F, Booking, A]

  def apply[F[_]: BookingAlgebra]: BookingAlgebra[F] = implicitly
}

class BookingInterpreter[F[_]: Monad] extends BookingAlgebra[F] {


  // adds room to booking, stores an event
  def addRoom(
               no: String,
               floor: Int,
               view: Boolean,
               capacity: Int
             ): BookingState[F, Room] = StateT[F, Booking, Room](booking => {
    val newRoom = RoomGenerator.generateRoom(no, floor, view, capacity)
    val newBooking = booking.copy(rooms = newRoom :: booking.rooms, events = RoomAdded(newRoom.no) :: booking.events)
    Monad[F].pure((newBooking, newRoom))
  })

  // returns current reservation id
  def currentReservationId(): BookingState[F, ReservationId] = StateT[F, Booking, ReservationId] {
    booking => (booking, booking.rooms.flatMap(_.booked).map(_.id).foldLeft(0)(math.max(_, _))).pure[F]
  }

  // fetches room by number
  def fetchRoom(no: String): BookingState[F, Option[Room]] = StateT[F, Booking, Option[Room]] { booking =>
    (booking.copy(events = RoomFetched(no) :: booking.events),
      booking.rooms.filter(_.no == no).headOption).pure[F]
  }

  // books a guest to a room for a given period
  def book(
            room: Room,
            period: Period,
            guest: Guest,
            reservationId: ReservationId): BookingState[F, Unit] = StateT[F, Booking, Unit] { booking => {
      val roomReserved = room.copy(booked = Reservation(reservationId, period, guest) :: room.booked)
      val otherRooms = booking.rooms.filter(_ != room)
      val newBooking = booking.copy(
        rooms = roomReserved :: otherRooms,
        events = ReservationMade(reservationId) :: booking.events
      )
      (newBooking, ()).pure[F]
    }
  }


}

object BookingProgram {

  // book vor VIP = book given room, if it does not exist then build it
  def bookVip[F[_]: Monad: BookingAlgebra](
               no: String,
               floor: Int,
               view: Boolean,
               capacity: Int,
               period: Period
             )(guest: Guest): BookingState[F, ReservationId] = for {
    maybeRoom <- BookingAlgebra[F].fetchRoom(no)
    room <- maybeRoom.fold(BookingAlgebra[F].addRoom(no, floor, view, capacity))((room:Room)  => room.pure[StateT[F, Booking, ?]])
    currentResId <- BookingAlgebra[F].currentReservationId
    resId = currentResId + 1
    _ <- BookingAlgebra[F].book(room, period, guest, resId)
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

