package db

import scalaz.{Applicative, Equal}
import scalaz.Isomorphism.IsoSet
import scalaz.scalacheck.ScalazProperties.applicative
import scalaz.syntax.apply._
import org.scalacheck.{Arbitrary, Prop}

trait =:!=[A, B]

object `=:!=` {
  implicit def neq[A, B]: A =:!= B = new =:!=[A, B] {}
  implicit def neqAmbig1[A]: A =:!= A = ???
  implicit def neqAmbig2[A]: A =:!= A = ???
}

trait Parser[A] {}

trait Table[A] {

  def columns: Columns[A]
  def parser: Parser[A]
}

sealed abstract class Column[A, K, B] { self =>
  def key: K
  def name: List[String]
  def read: A => B
  def any: Column[A, Any, B] = new Column[A, Any, B] {
    def key = self.key.asInstanceOf[Any]
    def name = self.name
    def read = self.read
  }
}

object Column {

  def apply[A, K, B](_name: String, _key: K, _read: A => B)(implicit ev: (A =:!= B)): 
  Column[A, K, B] =
    new Column[A, K, B] {
      def key = _key
      def name = List(_name)
      def read = _read
    }

  def applicative[AA] = new Applicative[({type F[B] = Column[AA, Any, B]})#F] {
    def point[A](a: => A): Column[AA, Any, A] = new Column[AA, Any, A] {
      def key = null
      def name = List()
      def read = (_: AA) => a
    }

    def ap[A, B](fa: => Column[AA, Any, A])(f: => Column[AA, Any, A => B]): 
    Column[AA, Any, B] =
      new Column[AA, Any, B] {
        def key = null
        def name = fa.name ++ f.name
        def read = (aa: AA) => f.read(aa)(fa.read(aa))
      }
  }
}

// constructive proof that there are exhaustive column defs for every field on A
trait Columns[A] {
  val col: Column[A, Any, A]
  lazy val iso = IsoSet(col.read, col.read)
}

object Columns {
  def apply[A](_col: Column[A, Any, A]) = new Columns[A] {
    val col = _col
  }
}

trait Condition[A] {}

object Condition {

}

trait Read[A] {
  def table: Table[A]
  def condition: Condition[A]
}

trait Create[A] {
  def table: Table[A]
  def create: A
}

trait Update[A] {
  def table: Table[A]
  def update: A
  def create: A
}

trait Delete[A] {
  def table: Table[A]
  def delete: A
}

object Example extends App {

  case class Data(key: String, value: Int)
  object Data {
    object key
    object value
    type CF[A] = Column[Data, Any, A]
    implicit val cda: Applicative[CF] = Column.applicative[Data]
    implicit val cKey = Column("key", key, (_: Data).key)
    implicit val cValue = Column("value", value, (_: Data).value)
    implicit val columns = Columns(
      ((cKey.any: CF[String]) |@| 
       (cValue.any: CF[Int]))(Data.apply _))
  }

  implicit def arbCd[B: Arbitrary] = Arbitrary { for {
    s <- Arbitrary.arbitrary[String] 
    b <- Arbitrary.arbitrary[B]
  } yield Column(s, null.asInstanceOf[Any], (_: Data) => b) }
  implicit val arbData = Arbitrary { for {
    k <- Arbitrary.arbitrary[String]
    v <- Arbitrary.arbitrary[Int]
  } yield Data(k, v) }
  implicit val eqCdi = new Equal[Column[Data, Any, Int]] {
    def equal(c1: Column[Data, Any, Int], c2: Column[Data, Any, Int]) = {
      val data = Arbitrary.arbitrary[Data].sample.get
      c1.name == c2.name && c1.read(data) == c2.read(data)
    }
  }
  applicative.laws[Data.CF].check
  Prop.forAll((d: Data) => Data.columns.iso.to(d) == d && Data.columns.iso.from(d) == d)
    .label("data.columns.iso").check
}
