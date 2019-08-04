import cats.Show
import cats.implicits._

object Lib {

  implicit class Pair2List[A](lhs : (A,A)){
    def toList : List[A] = List(lhs._1, lhs._2)
  }

  implicit def showTuple3[A : Show, B : Show, C : Show] : Show[(A, B, C)] = {
    case (a, b, c) => "( " + implicitly[Show[A]].show(a) + ", " + implicitly[Show[B]].show(b) + ", " + implicitly[Show[C]].show(c) + ")"
  }

  implicit class Tuple2Util[A, B](t : (A, B)){
    def fstUpdated[R](f : A => R) : (R, B) = (f(t._1), t._2)
    def sndUpdated[R](f : B => R) : (A, R) = (t._1, f(t._2))
  }


  def factorial(k : Int) : Long = if(k >= 2) (2 to k).product else 1

  //n >= k
  def binom(n : Int, k : Int) : Long ={
    (n.toLong to (n.toLong - k.toLong + 1) by -1).product / factorial(k)
  }

  def tailOption[A](l : List[A]) : Option[List[A]] = {
    l match{
      case _ :: xs => Some(xs)
      case _ => None
    }
  }

  implicit class ListUtils[A](lhs : List[A]){
    def tailOption : Option[List[A]] = Lib.tailOption(lhs)
  }

}
