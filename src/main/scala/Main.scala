import BookingSystem._

object Main extends App {

  val booking = Booking(List(
    Room("1", 0, view = true, capacity = 5, price = 100, rating = 3.2, booked = false),
    Room("2", 0, view = true, capacity = 3, price = 150, rating = 9.2, booked = true),
    Room("3", 0, view = false, capacity = 3, price = 120, rating = 8.4, booked = false),
    Room("4", 0, view = true, capacity = 4, price = 140, rating = 7.2, booked = false),
    Room("5", 0, view = true, capacity = 4, price = 140, rating = 4.6, booked = false)
  ))

  val best: Room = proposeBest(booking)
  println(best)
  assert(best.no == "4")

}
