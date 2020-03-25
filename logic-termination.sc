def sumOfAllNumbers(): Int =
  Array(10,20,30,40,50).sum

// -- sanity check
sumOfAllNumbers() == 150

def evenElements[A](ls: List[A]): List[A] =
  ls.view.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).toList

// -- sanity check
evenElements(List(1, 2, 3, 4, 5)) == List(2, 4)

def isPalindrome[A](ls: List[A]): Boolean =
  ls.reverse == ls

// -- sanity check
isPalindrome(List(1, 2, 1)) == true
isPalindrome(List(1)) == true
isPalindrome(List(1, 2, 2, 1)) == true

def largest[A <% Ordered[A]](ls: List[A]): A =
  ls.max

// -- sanity check
largest(List(1, 5, 2)) == 5
largest(List(5, 1, 2)) == 5
largest(List(2, 1, 5)) == 5

// -- get the nth Catalan number
// (1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862)
// -- start external context
object Succ { def unapply(i: Int) = if (i == 0) None else Some(i - 1) }
// -- end external context
val CatalanNumbers = Stream.unfold(0 -> 1l) {
  case (0, c) => Some(c -> (1, 1l))
  case (Succ(n), cn) =>
    val c = cn * (4 * n + 2) / (n + 2)
    Some(c -> (n + 2, c))
}
def catalan(i: Int): Long = CatalanNumbers(i)
// -- sanity check
catalan(0) == 1
catalan(1) == 1
catalan(3) == 5
catalan(6) == 132

// -- start external context
val data: Iterator[Int] = List(1, 2, 3).iterator // emulate input from environment
sealed trait Result
case class Data(data: Int) extends Result
case object Done extends Result
case class Error(message: String) extends Result
def receive: Result = if (data.hasNext) Data(data.next) else Done
// -- end external context
def getAll(): Option[List[Int]] = {
  var last: Option[Result] = None
  val data = Stream.continually(receive)
    .tapEach(s => last = Some(s))
    .takeWhile(_.isInstanceOf[Data])
    .foldLeft(List.empty[Int]) {
      case (ds, Data(d)) => ds :+ d
    }
  last.fold(Option.empty[List[Int]]) {
    case e: Error => None
    case _ => Some(data)
  }
}
// -- sanity check
getAll() == List(1, 2, 3)
