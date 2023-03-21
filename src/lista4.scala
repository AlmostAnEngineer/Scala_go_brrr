// Jacek Pasiecki 256057

import scala.annotation.tailrec

// Zadanie 1

def existsA[A] (xs: List[A]) (p: A => Boolean): Boolean =
  xs match 
    case h::t => p(h) || existsA(t)(p)
    case Nil => false

existsA (List(5,1,2,3)) (_ == 2)
!existsA (List(5,1,0,3)) (_ == 2)
!existsA(Nil)(_==0)
existsA (List("Ala", "ma", "kota"))(_ == "kota")

def existsB[A] (xs: List[A])(p: A => Boolean): Boolean =
  xs.foldLeft(false)((accumulator,element) => accumulator || p(element))

existsB (List(5,1,2,3)) (_ == 2)
!existsB (List(5,1,0,3)) (_ == 2)
!existsB(Nil)(_==0)
existsB (List("Ala", "ma", "kota"))(_ == "kota")

def existsC[A] (xs: List[A])(p: A => Boolean): Boolean =
  xs.foldRight(false)((element,accumulator) => accumulator || p(element))

existsC (List(5,1,2,3)) (_ == 2)
!existsC (List(5,1,0,3)) (_ == 2)
!existsC(Nil)(_==0)
existsC (List("Ala", "ma", "kota"))(_ == "kota")

// Zadanie 2 

def filter[A](xs: List[A])(p: A => Boolean): List[A] = 
  xs.foldRight[List[A]](Nil)
                       ((element, accumulator) => 
                          if p(element) then element :: accumulator else accumulator)

filter (List(2,7,1,3,7,8,4,1,6,9)) (_ > 3) == List(7, 7, 8, 4, 6, 9)
filter (List(1,2,3,4,5,6,7,8,9,10)) (_ > 5) == List(6,7,8,9,10)
filter (Nil)(_ == 10) == Nil
filter (List("Ala", "ma", "kota"))(_ == "kota") == List("kota")

// Zadanie 3
//Napisz funkcję remove1[A](xs: List[A])(p: A => Boolean): 
//List[A] zwracającą listę z tymi samymi wartościami, co lista xs, \
//z której usunięto pierwszy element spełniający predykat p.

def remove1A[A](xs: List[A])(p: A => Boolean): List[A] = 
   xs match 
    case h::t => if !p(h) then h::remove1(t)(p) else t
    case Nil => Nil
   
remove1A(List(1,2,3,2,5)) (_ == 2) == List(1, 3, 2, 5)
remove1A(List(1,1,1,1))(_ == 1) == List(1,1,1)
remove1A(Nil)(_ == 2) == Nil
remove1A(List("Ala", "ma", "kota")) (_ == "Ala") == List("ma", "kota")

def remove1B[A](xs: List[A])(p: A => Boolean): List[A] = 
  def remove1B_internal(xs: List[A], result: List[A]): List[A] =
    xs match 
      case h :: t => if p(h) then t.reverse_:::(result) else remove1B_internal(t, h::result)
      case Nil => result.reverse
  remove1B_internal(xs, Nil)

remove1B(List(1,2,3,2,5)) (_ == 2) == List(1, 3, 2, 5)
remove1B(List(1,1,1,1))(_ == 1) == List(1,1,1)
remove1B(Nil)(_ == 2) == Nil
remove1B(List("Ala", "ma", "kota")) (_ == "Ala") == List("ma", "kota")

def splitAt[A](xs: List[A])(n: Int): (List[A], List[A]) = 
  def getPairs(xs: List[A], n: Int, accumulator: List[A]): (List[A], List[A]) =
    xs match 
      case h::t => if n == 0 then (accumulator.reverse, xs) 
                      else getPairs(t, n-1, h::accumulator)
      case Nil => (accumulator.reverse, Nil)
    
  getPairs(xs,n,Nil)

splitAt(List(1,2,3,4))(2) == (List(1,2), List(3,4))
splitAt (List('a','b','c','d','e')) (2) == (List('a', 'b'), List('c', 'd', 'e'))
