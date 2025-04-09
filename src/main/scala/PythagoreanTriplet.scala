object PythagoreanTriplet:
   private lazy val pl = {
      for
         a <- 1 to 300
         b <- 1 to 400
         c <- 1 to 500
      yield (a, b, c)
   }
      .filter(t => t._1 < t._2 && t._2 < t._3 && isPythagorean(t) && t._1 + t._2 + t._3 <= 1000)

   def isPythagorean(t: (Int, Int, Int)): Boolean =
      t._1 * t._1 + t._2 * t._2 == t._3 * t._3

   def pythagoreanTriplets(n1: Int, n2: Int): Seq[(Int, Int, Int)] =
      pl.filter(t => t._1 >= n1 && t._3 <= n2)

   def pythagoreanTripletsSum(n: Int): Seq[(Int, Int, Int)] =
      pl.filter(t => t._1 + t._2 + t._3 == n)
