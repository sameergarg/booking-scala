import java.time.LocalDate

import BookingAlgebra._
import BookingProgram._
import Domain.{Booking, Guest, Period, ReservationId}
import cats.Id
import org.scalatest.{Matchers, WordSpec}

class BookingProgramSpec extends WordSpec with Matchers {

  "Booking program" should {

    "book vip room" in {
      implicit val bookingInterpreter = new BookingInterpreter[Id]
      val reservationPeriod = Period(LocalDate.of(2018, 1, 1), LocalDate.of(2019, 1, 1))
      val bookingState: BookingState[Id, ReservationId] = bookVip[Id]("1", 22, true, 2, reservationPeriod)(
        Guest("John", "Major"))

      val (booking, reservationId) = bookingState.run(Booking())

      reservationId shouldBe 1
      booking.rooms.flatMap(_.booked.map(_.period)) shouldBe List()
    }
  }
}
