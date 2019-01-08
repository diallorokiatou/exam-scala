package com.fr.upem.partiel

import java.time.Instant
import java.time.temporal.ChronoUnit.YEARS

import com.fr.upem.partiel.Part1.Converter


import scala.util.Try


// Part 1 (10pts)
object Part1 {

  // 1.1 Apply 'mul2' using pattern matching to the given integer (.5pts)
  def mul2(i: Int): Int = i * 2
  def applyMul2WithPatternMatching(i: Option[Int]): Option[Int] = i match {
    case None => None
    case Some(x) => Some(2 * x)
  }

  // 1.2 Apply 'mul2' WITHOUT using pattern matching to the given integer (.5pts)
  def parseOption(i : Option[Int]) : Int = i match {
    case None => 0
    case Some(x) => x
  }
  def applyMul2WithoutPatternMatching(i: Option[Int]): Option[Int] = if(i==None) None else Some(2 * parseOption(i))

  // 1.3 Refactor the following code using pattern matching (1pts)
  sealed trait Animal
  case object Cat extends Animal
  case object Bird extends Animal
  case class Dog(age: Int) extends Animal

  def formatAnimal(animal: Animal): String = animal match {
    case Cat =>  "It's a cat"
    case  Bird =>   "It's a bird"
    case x if(x.isInstanceOf[Dog]) =>  s"It's a ${animal.asInstanceOf[Dog].age} year old dog"
    case _ => throw new RuntimeException("This should not happen but I'm a Java developer !")
  }


  // 1.4 Find the index of the given element if any, use recursivity (1pts)
  def indexOf[A](l: List[A], a: A): Option[Int] =  l.zipWithIndex.filter{
                                    case (x,i) => x==a
                                  }.map{
                                    case (x,i) => i
                                      } match {
                                        case Nil => None
                                        case x => Some(x.head)
                                      }


  // 1.5 Throw away all errors (.5pts)
  case class Error(message: String)

  def parserEither[A](l : Either[Error, A]) : A = l match {
    case Right(x) => x
    case Left(x) => asInstanceOf
  }

  def keepValid[A](l: List[Either[Error, A]]): List[A] = l match {
    case Nil => Nil
    case x::xs if x == Left(Error("err")) =>  keepValid(xs)
    case x::xs => parserEither(x)::keepValid(xs)
  }

  // 1.6 Aggregate values (.5pts)
  def aggregate[A](l: List[A], combine: (A, A) => A, empty: A): A = l match {
    case Nil => empty
    case x::xs => xs.foldLeft(x)(combine)
  }

  // 1.7 Aggregate valid values (.5pts)
  def aggregateValid[A](l: List[Either[Error, A]], combine: (A, A) => A, empty: A): A = l match {
    case Nil => empty
    case _ => aggregate(keepValid(l),combine,empty)
  }

  // 1.8 Create the Monoid typeclass and rewrite the above "aggregateValid" (.5pts)
  sealed trait Monoid[A] {
    def identity: A
    def combine(a1: A, a2: A): A
  }

 implicit def aggregateValidM  : Monoid[Int] = new Monoid[Int] {
    def identity: Int = identity
    def combine(a1: Int, a2: Int): Int = a1 + a2
  }

  def aggregateValidMonoid[A](l: List[Either[Error, A]])(implicit ev : Monoid[A]): A = l match {
    case Nil => ev.identity
    case _ => aggregate(keepValid(l),ev.combine,ev.identity)
  }


  // 1.9 Implement the Monoid typeclass for Strings and give an example usage with aggregateValidM (.5pts)
  implicit def aggregateValidString  : Monoid[String] = new Monoid[String] {
    def identity: String = identity
    def combine(a1: String, a2: String): String = a1 + a2
  }


  // 1.10 Refactor the following object oriented hierarchy with an ADT (1.5pts)
 sealed trait FinancialAsset {
    def computeEarnings: Double
  }

  sealed trait  FlatRateAsset extends FinancialAsset {
    def rate: Double
    def amount: Double

    override def computeEarnings: Double = amount + (amount * rate)
  }

  case object LivretA {
    def Rate: Double = 0.75
  }

  case class LivretA(amount: Double) extends FlatRateAsset {
     def rate: Double = LivretA.Rate
  }

  case object Pel {
    def Rate: Double = 1.5
    def GovernmentGrant: Int = 1525
  }

  case class Pel(amount: Double, creation: Instant) extends FlatRateAsset {
    def rate: Double = Pel.Rate
    override def computeEarnings: Double = if (Instant.now().minus(4, YEARS).isAfter(creation))
        super.computeEarnings + Pel.GovernmentGrant
      else
        super.computeEarnings
  }

  case object CarSale {
    def StateHorsePowerTaxation: Int = 500
  }
  case class CarSale(amount: Int, horsePower: Int) extends FinancialAsset {
    override def computeEarnings: Double = amount - (CarSale.StateHorsePowerTaxation * horsePower)
  }

  // 1.11 Extract the "computeEarnings" logic of the above hierarchy
  // into an "Earnings" typeclass and create the adequate instances (1.5pts)
  sealed trait Earnings[A] {
    def rate: Double
    def computeEarnings: Double
  }

  // 1.12 Rewrite the following function with your typeclass (.5pts)
  /*def computeTotalEarnings(assets: List[FinancialAsset])(implicit ev : Earnings[FinancialAsset]):Double = assets match {
    case Nil => 0
    case x::_ =>  assets.foldLeft(0)
  }*/

  // 1.13 Enrich the "String" type with an "atoi" extension method that parses the
  // given String to an Int IF possible (1pts)
  object Converter {
    implicit class StringP(val s:String) extends AnyVal {
      def atoi = Try(s.toInt)
    }
  }


}

object main extends App{
  import Converter._
  println("1".atoi)
}