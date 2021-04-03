package u05lab.code

import scala.annotation.tailrec
import scala.language.postfixOps // silence warnings

sealed trait List[A] {

  def head: Option[A]

  def tail: Option[List[A]]

  def append(list: List[A]): List[A]

  def foreach(consumer: (A) => Unit): Unit

  def get(pos: Int): Option[A]

  def filter(predicate: (A) => Boolean): List[A]

  def map[B](fun: (A) => B): List[B]

  def toSeq: Seq[A]

  def foldLeft[B](acc: B)(f: (B,A)=>B): B

  def foldRight[B](acc: B)(f: (A,B)=>B): B

  def flatMap[B](f: A => List[B]): List[B]

  def reverse(): List[A]

  def zipRight: List[(A,Int)]

  def partition(pred: A => Boolean): (List[A],List[A])

  def span(pred: A => Boolean): (List[A],List[A])

  def reduce(op: (A,A)=>A): A

  def takeRight(n: Int): List[A]

  def collect[B](op: PartialFunction[A,B]): List[B]

  // right-associative construction: 10 :: 20 :: 30 :: Nil()
  def ::(head: A): List[A] = Cons(head,this)
}

// defining concrete implementations based on the same template

case class Cons[A](_head: A, _tail: List[A])
  extends ListImplementation[A]

case class Nil[A]()
  extends ListImplementation[A]

// enabling pattern matching on ::

object :: {
  def unapply[A](l: List[A]): Option[(A,List[A])] = l match {
    case Cons(h,t) => Some((h,t))
    case _ => None
  }
}

// List algorithms
trait ListImplementation[A] extends List[A] {

  override def head: Option[A] = this match {
    case h :: t => Some(h)
    case _ => None
  }
  override def tail: Option[List[A]] = this match {
    case h :: t => Some(t)
    case _ => None
  }
  override def append(list: List[A]): List[A] = this match {
    case h :: t => h :: (t append list)
    case _ => list
  }
  override def foreach(consumer: (A)=>Unit): Unit = this match {
    case h :: t => {consumer(h); t foreach consumer}
    case _ => None
  }
  override def get(pos: Int): Option[A] = this match {
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t get (pos-1)
    case _ => None
  }
  override def filter(predicate: (A) => Boolean): List[A] = this match {
    case h :: t if (predicate(h)) => h :: (t filter predicate)
    case _ :: t => (t filter predicate)
    case _ => Nil()
  }
  override def map[B](fun: (A) => B): List[B] = this match {
    case h :: t => fun(h) :: (t map fun)
    case _ => Nil()
  }

  override def toSeq: Seq[A] = this match {
    case h :: t => h +: t.toSeq // using method '+:' in Seq..
    case _ => Seq()
  }

  override def foldLeft[B](acc: B)(f: (B,A)=>B): B = this match {
    case Cons(h,t) => t.foldLeft(f(acc,h))(f)
    case Nil() => acc
  }

  override def foldRight[B](acc: B)(f: (A, B) => B): B =
    this.reverse().foldLeft(acc)((acc,elem) => f(elem,acc))

  override def reverse(): List[A] =
    this.foldLeft(Nil[A].asInstanceOf[List[A]])((acc,elem) => Cons(elem,acc))

  override def flatMap[B](f: A => List[B]): List[B] = this match {
    case Cons(h,t) => f(h).append(t.flatMap(f))
    case Nil() => Nil()
  }

  override def zipRight: List[(A,Int)] = {
      @tailrec
      def _zipRight(list: List[A], index: Int = 0, res:List[(A, Int)] = List.nil): List[(A,Int)] = list match {
        case h::t => _zipRight(t, index+1, (h,index)::res)
        case _ => res
    }
    _zipRight(this).reverse()
  }

  //filtering by predicate and than do the same thing negating the predicate
  override def partition(pred: A => Boolean): (List[A],List[A]) = (filter(pred), filter(!pred(_)))

  override def span(pred: A => Boolean): (List[A],List[A]) = {
    @tailrec
    def _span(list: List[A], res:(List[A],List[A]) = (List.nil, List.nil)): (List[A],List[A]) = list match {
      case h::t => if (pred(h)) _span(t, (res._1.::(h), res._2)) else (res._1, list)
      case _ => res
    }
    _span(this)
  }

  /**
    *
    * @throws UnsupportedOperationException if the list is empty
    */
  override def reduce(op: (A,A) => A): A = this match {
    case h :: t =>
      var result = h
      for (elem <- t) {
        result = op(result, elem)
      }
      result
    case Nil() => throw new UnsupportedOperationException()
  }

  override def takeRight(n: Int): List[A] = span(_ != get(n).get)._2

  //filter by the predicate defined in partial function and map the filtered map
  override def collect[B](op: PartialFunction[A, B]): List[B] = filter(op.isDefinedAt).map(op)

}

object sequenceOnList {

  //I'm still thinking about this, cause I can see that it is kinda similar to foldLeft.
  //The main problem that at the moment I can't solve is that I don't want that the recursion of foldLeft continues
  //if it find a None element in the head.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def _sequence(a:List[Option[A]])(accumulator: Option[List[A]]): Option[List[A]]= a match {
      case h :: t => h match {
        case Some(v) =>
          _sequence(t)(Option(accumulator match {
            case None => List(v)
            case Some(value) => value.append(List(v))
          }))
        case _ => Option.empty
      }
      case _ => accumulator
    }
    _sequence(a)(Option.empty)
  }

  //This implementation uses foldLeft... I don't know if it's better, 'cause to stop the recursion I use
  //an exception.
  //I'm still thinking if I can find a better solution...
  def sequence1[A](a: List[Option[A]]): Option[List[A]] = try {
    a.foldLeft(Option.empty: Option[List[A]])((acc, elem) => elem match {
      case Some(v) =>
        acc match {
          case Some(value) => Option(value.append(List(v)))
          case _ => Option(List(v))
        }
    })
  } catch  {
    case _: MatchError => Option.empty
  }

}

// Factories
object List {
  // Smart constructors
  def nil[A]: List[A] = Nil()
  def cons[A](h: A, t: List[A]): List[A] = Cons(h,t)

  def apply[A](elems: A*): List[A] = {
    var list: List[A] = Nil()
    for (i <- elems.length-1 to 0 by -1) list = elems(i) :: list
    list
  }

  def of[A](elem: A, n: Int): List[A] =
    if (n==0) Nil() else elem :: of(elem,n-1)
}