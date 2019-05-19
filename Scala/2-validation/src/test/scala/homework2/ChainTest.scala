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
  
  it should "recognize two NOT equal chains" in {
    (Chain(1, 2, 5).equals(Chain(4)) shouldBe false)
  }
  
  
  "+:" should "add one front element" in {
    (Chain(1, 2).+:(3) shouldBe Chain(3, 1, 2)) 
  }
  
   ":+" should "add one back element" in {
    (Chain(1, 2).:+(3) shouldBe Chain(1, 2, 3)) 
  }
   
   "listify" should "OK make first element of an append be a singleton" in {
     (Append(Append(Singleton(1), Singleton(2)), Append(Singleton(3), Singleton(4))).listify shouldBe Append(Singleton(1), Append(Singleton(2), Append(Singleton(3), Singleton(4)))))
   }
   
   it should "OK apply listify on a Singleton" in {
     (Singleton(1).listify shouldBe Singleton(1))
   }
   
   it should "OK apply listify on Append of two Singletons" in {
     (Append(Singleton(1), Singleton(2)).listify shouldBe Append(Singleton(1), Singleton(2)))
   }
   
   it should "OK 2app" in {
     (Append(Append(Singleton(1), Singleton(2)), Singleton(3)).listify shouldBe Append(Singleton(1), Append(Singleton(2), Singleton(3))))
   }
   
   it should "OK 2 leveled listify" in {
     (Append(Append(Singleton(1), Append(Singleton(2), Singleton(3))), Singleton(4)).listify shouldBe 
     Append(Singleton(1), Append(Singleton(2), Append(Singleton(3), Singleton(4)))))
   }
   
      it should "SHOULD NOT PASS BUT PASSED  1 leveled listify" in {
     (Append(Append(Append(Singleton(1), Append(Singleton(2), Singleton(3))), Singleton(4)), Singleton(5)).listify shouldBe 
     Append(Singleton(1), Append(Append(Append(Singleton(2), Singleton(3)), Singleton(4)), Singleton(5))))
   }
      
      it should "test left" in {
     (Append(Append(Append(Singleton(1), Append(Singleton(2), Singleton(3))), Singleton(4)), Singleton(5)).left shouldBe 
     Append(Append(Singleton(1), Append(Singleton(2), Singleton(3))), Singleton(4)))
   }
      
      "tail" should "tail singleton" in {
        (Append(Singleton(1), Singleton(2)).tail.get shouldBe Singleton(2))
      }
      
      it should "tail Append of two Singletons" in {
        (Append(Singleton(1), Append(Singleton(2), Singleton(3))).tail.get shouldBe Append(Singleton(2), Singleton(3)))
      }
      
      it should "tail Append leveled with left Singleton" in {
        (Append(Singleton(1), Append(Append(Singleton(2), Singleton(3)), Singleton(4))).tail.get shouldBe Append(Append(Singleton(2), Singleton(3)), Singleton(4)))
      }
      
      it should "tail Append leveled with left Append" in {
        (Append(Append(Singleton(1), Singleton(2)), Singleton(3)).tail.get shouldBe Append(Singleton(2), Singleton(3)))
      }
      
      it should "Example from the boratd - The tail should BE like this" in {
        (Append(Append(Append(Singleton(1), Singleton(2)), Append(Singleton(3), Singleton(4))), Singleton(5)).listify shouldBe 
            Append(Singleton(1), Append(Singleton(2), Append(Singleton(3), Append(Singleton(4), Singleton(5))))))
      }
      
      "foldLeft" should "sum values" in {
        (Append(Singleton(1), Append(Singleton(2), Singleton(3))).foldLeft(0)(_ + _) shouldBe 6)
      }
      
      it should "calculate the product" in {
        (Append(Singleton(4), Append(Singleton(2), Singleton(3))).foldLeft(1)(_ * _) shouldBe 24)
      }
      
      "map" should "double values" in {
        (Append(Singleton(1), Append(Singleton(2), Singleton(3))).map(x => 2*x) shouldBe Append(Singleton(2), Append(Singleton(4), Singleton(6))))
      }
      it should "double value of singleton" in {
        (Singleton(3).map(x => 2*x) shouldBe Singleton(6))
      }
}
