package fp

object OptionExcercises {

  def variance(xs: Seq[Double]): Option[Double] = {
    val mean = if (xs.isEmpty) {
      None
    } else {
      val sum = xs.reduce(_+_)
      val mean = sum / xs.length
      Some(mean)
    }

    val varianceSum = mean
      .map(m => {
        xs
          .map(x => math.pow(x - m, 2))
          .reduce(_+_)
      })

    varianceSum.map(vs => vs / xs.length)
  }

}