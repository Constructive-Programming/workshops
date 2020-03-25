import scala.util.Try

def parseInt(number: String): Try[Int] = Try(number.toInt)
// -- sanity check
parseInt("oops")


def root(number: Int): Option[Int] = if (number < 1) None else Some(Math.sqrt(number.toDouble).toInt)
// -- sanity check
root(-1)

case class NonEmptyList[A](head: A, tail: List[A])
def head[A](list: NonEmptyList[A]): A = list.head
// -- sanity check
head(NonEmptyList(1, Nil))

def find[K, V](dictionary: Map[K, V], key: K): Option[V] = dictionary.get(key)
// -- sanity check
find(Map.empty[Int, String], 1)
find(Map(1 -> "Uno"), 2)

def safeSquare(i: Int): Either[Exception, Int] = {
  if (i > 46340) Left(new IllegalArgumentException("Input too high"))
  else if (i < -46340) Left(new IllegalArgumentException("Input too low"))
  Right(i * i)
}
// -- sanity check
safeSquare(46500)
