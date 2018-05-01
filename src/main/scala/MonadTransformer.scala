import cats.data.OptionT
import cats.implicits._

import scala.util.{Failure, Success, Try}

object MonadTransformer {

  def calculate(input: String): Validation[Int] = increment(input)

  val increment: String => Validation[Int] = str => {

    def parseToInt(i: String): Either[String, Option[Int]] = Try(i.toInt) match {
      case Success(i) => Some(i+1).asRight[String]
      case Failure(ex) => ex.getMessage.asLeft[Option[Int]]
    }

    OptionT[Error, Int](parseToInt(str))
  }


  type Error[V] = Either[String, V]
  type Validation[A] = OptionT[Error, A]

}
