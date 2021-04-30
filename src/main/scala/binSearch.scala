import scala.annotation.tailrec

object binSearch extends App {

  implicit class implicitList(l: List[Double]) {
    def binarySearch(a: Double): (Option[Int],Option[Int]) = {
      // https://pl.wikipedia.org/wiki/Wyszukiwanie_binarne
      @tailrec
      def Helper(a:Double,lewo: Int, prawo: Int): (Option[Int],Option[Int]) = {
        prawo - lewo match {
          case b if b > 1 =>
            val ind: Int = (prawo + lewo) / 2
            println(l(ind))
            l(ind) match {
              case i if i < a => Helper(a, ind, prawo)
              case _ => Helper(a, lewo, ind)
            }
          case _ =>
            if (l(lewo) > a) {
              (None, Some(lewo))
            } else if (l(prawo) < a) {
              (Some(prawo), None)
            } else {
              (Some(lewo), Some(prawo))
            }
        }
      }
      l match {
        case List() => (None,None)
        case _ => Helper(a,0,l.length-1)
      }
    }
  }
  val a: List[Double] = Range(0,1000,2).map(_.toDouble).toList
  println(a)
  println(a.binarySearch(-2))
}
