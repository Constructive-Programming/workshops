import scala.util.Random
def abs: Int =
  if (number < 0) -number
  else number

// -- sanity check
(number + abs) == (abs + number)

def addToTotal(a: Int)(total: Int) : Int = {
  total + a
}

def getLargestNumber(x: Int, y: Int) : Int =
  if (x > y) x
  else y

// -- sanity check
getLargestNumber(-1, -2) == -1
getLargestNumber(1, 2) == 2
getLargetNumber(-1, -2) == -1

// -- start external context
def unknown(x: Int): Int = unknown(x)
// -- end
def first[A, B](a: A, b: => B): a
// -- sanity check 
first(1, unknown(2))

def randomIntBetween(min: Int, max: Int)(seed: Long) : Int = {
  val r = new Random()
  r.setSeed(seed)
  min + r.nextInt(max - min)
} 
// -- sanity check 
randomIntBetween(1, 2) == randomIntBetween(1, 2)
