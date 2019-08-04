import cats.effect.IO
import cats.{Applicative, CommutativeApply, Foldable, Now, Show}
import cats.implicits._

import scala.util.Random

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


  //returns chosen object and all other objects
  //obj must be nonEmpty
  def chooseRandom[A](objs : List[A]) : (A, List[A]) = {
    val randIndex = new Random().nextInt(objs.size)
    def f(objs : List[A], i : Int = 0, others : List[A] = Nil) : List[A] = {
      objs match{
        case x :: xs if i != randIndex => f(xs, i + 1, x :: others)
        case x :: xs => x :: f(xs, i + 1, others)
        case Nil => others
      }
    }

    val headTail = f(objs)
    (headTail.head, headTail.tail)
  }

  def chooseRandom[A](objs : List[A], n : Int) : (List[A], List[A]) = {
    if(n > objs.size) throw new Exception

    if(n == 0) Nil -> objs
    else{
      val (a, as) = chooseRandom(objs)
      chooseRandom(as, n - 1).fstUpdated(a :: _)
    }

  }

  def parseBoolean(str : String) : Option[Boolean] = {
    try{
      Some(str.toBoolean)
    }catch{
      case _ : IllegalArgumentException => None
    }
  }

  def parseInt(str : String) : Option[Int] = {
    try{
      Some(str.toInt)
    }catch{
      case _ : NumberFormatException => None
    }
  }

  def putStrLn(str: String) : IO[Unit] = IO{println(str)}

}
