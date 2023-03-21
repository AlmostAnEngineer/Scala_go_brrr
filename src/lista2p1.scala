// Jacek Pasiecki 256057
// Zadanie 1

import scala.annotation.tailrec

def take[A](n: Int, xs: List[A]): List[A] =
    xs match
        case h::t => if n > 0 then h::take(n-1,t) else Nil
        case Nil => Nil

take(2, List(1,2,3,5,6)) == List(1,2) 
take(-2, List(1,2,3,5,6)) == Nil
take(8, List(1,2,3,5,6)) == List(1,2,3,5,6)
take(2, List("kota", "ma", "Ala"))

// Zadanie 2

def drop[A](n: Int, xs: List[A]): List[A] = 
    xs match
        case h :: t => if n > 0 then drop(n-1,t) else h :: drop(n-1,t)
        case Nil => Nil

drop(2, List(1,2,3,5,6)) == List(3,5,6) 
drop(-2, List(1,2,3,5,6)) == List(1,2,3,5,6) 
drop(8, List(1,2,3,5,6)) == Nil
drop(1,List())
drop(2,List("test1","test2","test3"))

// Zadanie 3

def reverse[A](xs: List[A]): List[A] = 
    @tailrec
    def helper(xss: List[A], acc: List[A]): List[A] = 
        xss match
            case h :: t => if xss != Nil then helper(t, h :: acc) else Nil
            case Nil => acc
    helper(xs,Nil)
    
reverse(List("Ala", "ma", "kota")) == List("kota", "ma", "Ala")
reverse(List("Ala", "ma", "kota")) == List("kota", "ma", "Ala")
reverse(List(1,2,3,4,5)) == List(5,4,3,2,1)

// Zadanie 4

val replicate: List[Int] => List[Int] = xs =>
    def times(a: Int, i: Int, tail: List[Int]): List[Int] = 
        if i > 0 then a::times(a,i-1,tail)
        else replicate(tail)
    xs match
        case h::t => times(h,h,t)
        case Nil => Nil

replicate (List(1,0,4,-2,3)) == List(1, 4, 4, 4, 4, 3, 3, 3)
replicate (List(1,2,3)) == List(1,2,2,3,3,3)
replicate (Nil) == Nil

// Zadanie 5

val root3: Double => Double = a =>
    val EPSILON: Double = 1e-15
    @tailrec
    def helper(xi: Double = 1): Double = 
        if Math.abs(xi*xi*xi - a) <= EPSILON * Math.abs(a) then xi
        else helper(xi + ((a/(xi*xi)-xi)/3))
        helper(if a > 1 then a/3 else a)

val epsilon: Double = 1e-60
Math.abs(root3(8)-2) <= epsilon
Math.abs(root3(-8)+2) <= epsilon
Math.abs(root3(0)) <= epsilon



    
    
