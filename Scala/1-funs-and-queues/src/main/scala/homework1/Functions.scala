package homework1
import scala.math.pow

object Functions {
  
  def fromDigits(digits: List[Int], radix: Int = 10): Int = 
  {
     def fromDigitsAcc(digits: List[Int], radix: Int, acc: Int): Int = 
     {
       if(digits.isEmpty)
       {
         acc
       }
       else
       {
         val step = calculateStep(digits, radix)
         fromDigitsAcc(digits.tail, radix, acc + step)
       }
     }
     
     fromDigitsAcc(digits, radix, 0)
  }
  
  def calculateStep(digits: List[Int], radix: Int): Int = 
  {
    digits.head * (pow(radix, digits.length - 1)).toInt
  }
  
  def parseInteger(integer: String, radix: Int = 10): Int = 
  {
    val integerAbsoluteValueAsString = if(integer.head == '-') integer.substring(1) else integer
    
    val digitsAsChars: List[Char] = integerAbsoluteValueAsString.toList
    val digits: List[Int] = digitsAsChars.map(x => charToInt(x))
    
    val fromDigitsAbsolute = fromDigits(digits, radix)
    
    if(integer.head == '-') -fromDigitsAbsolute else fromDigitsAbsolute
  }
  
  def charToInt(c: Char): Int = 
  {
    if(c.isDigit)
    {
      c.toInt - 48
    }
    else
    {
      c - 55
    }
  }
  
  def zipMap(a: List[Int], b: List[Int], f: (Int, Int) => Int): List[Int] = ???

  def countCoinChangeVariants(denominations: List[Int], change: Int): Int = ???

  def bfsTraversal(start: Int, end: Int, neighbours: Int => List[Int]): Queue = ???
}
