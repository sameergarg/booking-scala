
object MonadTransformer {

  def calculate(input: String): Option[Int] = ???

  val increment: Option[Int] = for {
    v <- calculate("some")
  } yield (v + 1)



}
