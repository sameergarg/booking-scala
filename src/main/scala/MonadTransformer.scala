
object MonadTransformer {

  def calculate(input: String): Validation[Option[Int]] = ???

  val increment: Option[Int] = ???


  type Validation[A] = Either[String, A]
}
