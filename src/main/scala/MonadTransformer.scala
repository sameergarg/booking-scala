import scala.util.{Failure, Success, Try}

object MonadTransformer {

  def calculate(input: String): Validation[Option[Int]] = Try(input.toInt) match {
    case Success(i) => Right(Some(i +1))
    case Failure(ex) => Left(ex.getMessage)
  }

  val increment: Option[Int] = {
    val incremented = for {
      inc <- calculate("some")
    } yield {
      inc
    }
    incremented.fold(_ => None, identity)
  }


  type Validation[A] = Either[String, A]
}
