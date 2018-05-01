import Domain._
import cats.implicits._
import cats.{Applicative, Functor, Monad}

import scala.language.higherKinds

object BookingOps {

  val pickAvailable: (Period, List[Room]) => List[Room] = (period, rooms) => rooms.filter(!_.booked.map(_.period).contains(period))
  val filterWithView: List[Room] => List[Room] = _.filter(_.view)
  val canAccommodate: (NoPpl, List[Room]) => List[Room] = (noPpl, rooms) => rooms.filter(_.capacity >= noPpl)

  val sortByRating: List[Room] => List[Room] = _.sorted

  def costPerPerson[F[_] : Functor]: F[Room] => F[Double] = _.map(room => room.price / room.capacity)

  // available & with View & has best rating
  val proposeBest: (Booking, Period, NoPpl) => Option[Room] = { (booking, period, noPpl) =>

    val roomWithCapacity: List[Room] => List[Room] = canAccommodate.curried(noPpl)
    val roomNotBooked: List[Room] => List[Room] = pickAvailable.curried(period)

    val best: List[Room] => Option[Room] = roomWithCapacity >>> roomNotBooked >>> filterWithView >>> sortByRating >>> (rooms => rooms.headOption)

    best(booking.rooms)
  }

  val costPerPersonForBest: (Booking, Period, NoPpl) => Option[Double] = Function.untupled(proposeBest.tupled >>> costPerPerson)

  val isAffordable: (Room, Price) => Boolean = (room, affordablePrice) => room.price < affordablePrice

  def affordableFor[F[_] : Applicative](room: F[Room], price: Price): F[Boolean] = {
    val fPriceToAff = room.map(isAffordable.curried)
    fPriceToAff <*> price.pure[F]
  }

  def affordableFor[F[_] : Applicative](room: F[Room], price: F[Price]): F[Boolean] =
    (room, price).mapN(isAffordable)

  //same as propose best but all arguments are wrapped in effect
  def bestFor[F[_]: Monad](
                                  booking: F[Booking],
                                  fetchPeriod: Booking => F[Period],
                                  fetchNoPpl: Booking => F[NoPpl]
                                ): F[Option[Room]]  =
    for {
      b       <- booking
      period  <- fetchPeriod(b)
      noPpl   <- fetchNoPpl(b)
    } yield proposeBest(b, period, noPpl)
}

