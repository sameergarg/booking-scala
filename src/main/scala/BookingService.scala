import Domain.Event.{ReservationMade, RoomAdded, RoomFetched}
import Domain._
import cats.{Applicative, Monad}
import cats.data.{State, StateT}
import cats.implicits._

import language.higherKinds

object BookingService {

  type BookingState[F[_], A] = StateT[F, Booking, A]

  // adds room to booking, stores an event
  def addRoom[F[_]: Monad](
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
  private def currentReservationId[F[_]: Monad]: BookingState[F, ReservationId] = StateT[F, Booking, ReservationId] {
    booking => (booking, booking.rooms.flatMap(_.booked).map(_.id).foldLeft(0)(math.max(_, _))).pure[F]
  }

  // fetches room by number
  def fetchRoom[F[_]: Monad](no: String): BookingState[F, Option[Room]] = StateT[F, Booking, Option[Room]] { booking =>
    (booking.copy(events = RoomFetched(no) :: booking.events),
      booking.rooms.filter(_.no == no).headOption).pure[F]
  }

  // books a guest to a room for a given period
  def book[F[_]: Monad](
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

  // book vor VIP = book given room, if it does not exist then build it
  def bookVip[F[_]: Monad](
               no: String,
               floor: Int,
               view: Boolean,
               capacity: Int,
               period: Period
             )(guest: Guest): BookingState[F, ReservationId] = for {
    maybeRoom <- fetchRoom[F](no)
    room <- maybeRoom.fold(addRoom[F](no, floor, view, capacity))((room:Room)  => room.pure[StateT[F, Booking, ?]])
    currentResId <- currentReservationId[F]
    resId = currentResId + 1
    _ <- book[F](room, period, guest, resId)
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

