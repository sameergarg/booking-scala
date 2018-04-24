
case class Room(
                 no: String,
                 floor: Int,
                 view: Boolean,
                 capacity: Int,
                 price: Double,
                 rating: Double,
                 booked: Boolean)

case class Booking(rooms: List[Room])

object Functions {

  val costPerPerson: Room => Double = room => room.price / room.capacity

  val pickAvailable: List[Room] => List[Room] = _.filter(!_.booked)
  val filterWithView: List[Room] => List[Room] = _.filter(_.view)
  val sortByRating: List[Room] => List[Room] = _.sortBy(_.rating)

  // available & with View & has best rating
  val proposeBest: Booking => Room = ???

}

