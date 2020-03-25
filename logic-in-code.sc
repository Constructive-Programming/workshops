import $ivy.`org.typelevel::cats-core:2.1.0`
import cats.implicits._

def parseInt(number: String): Int = number.toInt
def root(number: Int): Int = Math.sqrt(number.toDouble).toInt
def head[A](list: List[A]): A = list.head
def find[K, V](dictionary: Map[K, V], key: K): V = dictionary(key)

def abs(number: Int): Int = {
  var abs = number;
  if (abs < 0) abs = -abs;
  abs
}


// -- start external context
val data: Iterator[Int] = List(1, 2, 3).iterator // emulate input from environment
sealed trait Result
case class Data(data: Int) extends Result
case object Done extends Result
case class Error(message: String) extends Result
def receive: Result = if (data.hasNext) Data(data.next) else Done
// -- end external context
def getAll(): Option[List[Int]] = {
  def accumulate(accum: List[Int]): Option[List[Int]] =
    receive match {
      case Data(data) => accumulate(data :: accum)
      case Done => Some(accum.reverse)
      case Error(_) => None
    }

  accumulate(Nil)
}

def getAll2(): Option[List[Int]] = {
  Stream.continually(receive).takeWhile(_.isInstanceOf[Data]).foldMapA {
    case Data(data) => Some(List(data))
    case Done => Some(Nil)
    case Error(_) => None
  }
}

println(getAll2())

abs(1)

find[String, Int](Map.empty, "item")
head[Int](Nil)
parseInt("failed")
root(-1)


def insertionSort[A <% Ordered[A] : scala.reflect.ClassTag](data: List[A]): List[A] = {
    val n: Int = data.length
    val array = data.toArray
    for (j <- 1 until n) {
      val key = array(j);
      var i = j - 1;
      while ( (i > -1) && ( array(i) > key ) ) {
        array(i+1) = array(i);
        i -= 1;
      }
      array(i+1) = key;
    }
  array.toList
  }
