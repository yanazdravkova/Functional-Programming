package homework1

import org.scalatest.{FlatSpec, Matchers}

class QueueTest extends FlatSpec with Matchers {
  
  "an empty queue" should "produce a queue with a single element when that element is added to it" in {
    val emptyQueue = Queue.empty
    val singleElementQueue = emptyQueue.push(42)

    singleElementQueue.peek shouldBe 42
    singleElementQueue.size shouldBe 1
  }
  
  it should "produce a queue with two elements when these elements are added to it" in {
    val emptyQueue1 = Queue.empty
    val twoElementQueue = emptyQueue1.push(13).push(7)

    twoElementQueue.peek shouldBe 13
    twoElementQueue.size shouldBe 2
  }
    
  "apply" should "produce a queue from List" in {
    val queueFromList = Queue(List(1,7, 8))
    queueFromList.peek shouldBe 8
    queueFromList.size shouldBe 3
  }
  
  it should "produce a queue from Range" in {
    val queueFromRange = Queue(0 to 8)
    queueFromRange.peek shouldBe 8
    queueFromRange.size shouldBe 9
    queueFromRange.pop.size shouldBe 8    
  }
  
  "pop" should "produce an empty queue after pop from single element queue" in {
    val emptyQueue = Queue.empty
    val singleElementQueue = emptyQueue.push(42)
    singleElementQueue.pop.isEmpty shouldBe true
  }  

}
