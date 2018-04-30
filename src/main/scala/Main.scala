import java.time.LocalDate

import BookingService.BookingState
import Domain._
import IOOperations._

object Main extends App {

  val guest = Guest("John", "Major")
  val period = Period(LocalDate.of(2017, 1, 8), LocalDate.of(2017, 1, 12))
  private val program: BookingState[List[ReservationId]] = for {
    resId1 <- BookingService.bookVip("101", floor = 1, view = true, capacity = 5, period)(guest)
    resId2 <- BookingService.bookVip("102", floor = 1, view = true, capacity = 5, period)(guest)
  } yield List(resId1, resId2)

  val reservationIds = for {
    initBooking <- fetchBooking()
    (booking, resIds) = program.run(initBooking).value
    _ <- updateBooking(booking)
  } yield resIds

  reservationIds.unsafeRunAsync(_.foreach(println))

}
