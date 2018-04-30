import Domain.Booking
import cats.effect.{IO, Sync}

object IOOperations {

  private object InMemoryDB {
    //has to be var as it needs to capture changing state
    var booking: Booking = new Booking()
  }

  val fetchBooking: () => IO[Booking] = () => Sync[IO].delay {
    InMemoryDB.booking
  }
  val updateBooking: Booking => IO[Unit] = (booking: Booking) => Sync[IO].delay {
    InMemoryDB.booking = booking
  }
}
