// Jacek Pasiecki 256057
// Zadanie 1

def suma(list: List[Double]): Double = 
    if list != Nil then list.head + suma(list.tail)
    else 0
suma(Nil) == 0.0
suma(List(-1, 2, 3)) == 4.0 
suma(List(5.6)) == 5.6

// Zadanie 2

def last[A](xs: List[A]): A = 
    if xs.tail != Nil then last(xs.tail)
    else xs.head


def ends[A](xs: List[A]): (A, A) = 
    if xs == Nil then throw new NoSuchElementException("empty list")
    else if xs.tail == Nil then (xs.head,xs.head)
    else (xs.head,last(xs.tail))

ends(List(1, 3, 5, 6, 9)) == (1,9) 
ends(List("Ala", "ma", "kota")) == ("Ala", "kota") 
ends(List(1)) == (1,1)
//ends(Nil) =>> wyjątek NoSuchElementException: empty list
// Zadanie 3

def posortowana(xs: List[Int]):Boolean = 
    if xs != Nil & xs.tail != Nil then true
    else xs.head <= xs.tail.head && posortowana(xs.tail)


posortowana(List(1,2,3)) == true
posortowana(Nil) == true

// Zadanie 4

def glue(xs: List[String],sep: String): String = 
    if xs == Nil then ""
    else if xs.tail == Nil then xs.head 
    else s"${xs.head}$sep${glue(xs.tail,sep)}"

glue(List("To", "jest", "napis"), "-") == "To-jest-napis" 
glue(Nil, "-") == ""