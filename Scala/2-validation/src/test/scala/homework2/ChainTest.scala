package homework2

import org.scalatest.{FlatSpec, Matchers}

class ChainTest extends FlatSpec with Matchers {
  "++" should "append two chains" in {
    (Chain(1, 2) ++ Chain(3, 4)) shouldEqual Chain(1, 2, 3, 4)
  }
  
  "isEmpty" should "recognize as NON EMPTY chain" in {
    (Chain(1, 2).isEmpty shouldBe false)
  }
  
  "equals" should "recognize two equal chains" in {
    (Chain(1, 2, 3).equals(Chain(1, 2, 3)) shouldBe true)
  }
  
  "equals" should "recognize two NOT equal chains" in {
    (Chain(1, 2, 5).equals(Chain(4)) shouldBe false)
  }
  
  
  "+:" should "add one front element" in {
    (Chain(1, 2).+:(3) shouldBe Chain(3, 1, 2)) 
  }
  
   ":+" should "add one back element" in {
    (Chain(1, 2).:+(3) shouldBe Chain(1, 2, 3)) 
  }
}
