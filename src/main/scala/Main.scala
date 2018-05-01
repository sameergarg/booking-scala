import java.time.LocalDate

import BookingAlgebra.BookingState
import Domain._
import IOOperations._
import cats.effect.IO
import BookingProgram._

object Main extends App {

  val guest = Guest("John", "Major")
  val period = Period(LocalDate.of(2017, 1, 8), LocalDate.of(2017, 1, 12))

  implicit val bookingInterpreter = new BookingInterpreter[IO]

  private val program: BookingState[IO, List[ReservationId]] = for {
    resId1 <- bookVip[IO]("101", floor = 1, view = true, capacity = 5, period)(guest)
    resId2 <- bookVip[IO]("102", floor = 1, view = true, capacity = 5, period)(guest)
  } yield List(resId1, resId2)

  //interpreter
  val reservationIds = for {
    //guest       <- findGuest
    initBooking <- fetchBooking()
    bookingAndResIds <- program.run(initBooking)
    (booking, resIds) = bookingAndResIds
    _ <- updateBooking(booking)
  } yield resIds

  reservationIds.unsafeRunAsync(_.foreach(println))

  println(MonadTransformer.calculate("some").value)
  println(MonadTransformer.calculate("1").value)
}
