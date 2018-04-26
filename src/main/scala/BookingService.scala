import Domain._

class BookingService {

  // adds room to booking, stores an event
  def addRoom(
               no: String,
               floor: Int,
               view: Boolean,
               capacity: Int
             ): Room = ???

  // returns current reservation id
  def currentReservationId: ReservationId = ???

  // fetches room by number
  def fetchRoom(no: String): Option[Room] = ???

  // books a guest to a room for a given period
  def book(
            room: Room,
            period: Period,
            guest: Guest,
            reservationId: ReservationId): Unit = ???

  // book vor VIP = book given room, if it does not exist then build it
  def bookVip(
               no: String,
               floor: Int,
               view: Boolean,
               capacity: Int,
               period: Period
             )(guest: Guest): ReservationId = ???

}
