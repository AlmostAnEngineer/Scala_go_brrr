// Pasiecki Jacek
// Zadanie 1
def last[A](xs: List[A]): A = xs match 
    case head :: Nil => head
    case _ :: tail => last(tail)
    case Nil => throw new NoSuchElementException("empty list")
def test[A](list: List[A],eq:A): Unit = 
    val result = last(list)
    if (result != eq) 
        println(s"Test failed: expected $eq but got $result")
    else 
        println("Test passed")
object L0{ 
    def main(args: Array[String]): Unit = {
        //println(last(List(1,2,3,4,55)))
        test(List('A','B'),'B')
        test(List(1,2,3,4,5,3),3)
        test(List('a','c','o','o'),'o')
        test(List("Ala", "ma", "kota"),"kota")
        test(List(1),1)
        //println(last(Nil))
        }
    }