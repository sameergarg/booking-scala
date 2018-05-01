import Domain.{Booking, Guest}
import cats.effect.{IO, Sync}

object IOOperations {

  type GuestId = Int
  private object InMemoryDB {
    //has to be var as it needs to capture changing state
    var booking: Booking = new Booking()

    var guests: Map[GuestId, Guest] = Map(1 -> Guest("John", "Major"))
  }

  val fetchBooking: () => IO[Booking] = () => Sync[IO].delay {
    InMemoryDB.booking
  }

  val updateBooking: Booking => IO[Unit] = (booking: Booking) => Sync[IO].delay {
    InMemoryDB.booking = booking
  }

  val findGuest: GuestId => IO[Guest] = id => Sync[IO].delay {
    InMemoryDB.guests(id)
  }
}
