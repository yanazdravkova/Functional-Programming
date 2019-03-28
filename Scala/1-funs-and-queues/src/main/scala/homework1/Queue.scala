package homework1

class Queue(val inbox: List[Int], val outbox: List[Int]) 
{
  def peek: Int = 
  {
    if(isEmpty) throw new NoSuchElementException
    else if(outbox.isEmpty) inbox.last
    else outbox.head
  }
  
  def push(n: Int): Queue = 
  {
    new Queue(n::inbox, outbox)
  }
  
  def pop: Queue = 
  {
    if(isEmpty) throw new NoSuchElementException
    else if(outbox.isEmpty) new Queue(List.empty, inbox.reverse.tail)
    else new Queue(inbox, outbox.tail)
  }

  def isEmpty: Boolean = 
  {
    inbox.isEmpty && outbox.isEmpty
  }
  
  def size: Int = 
  {
    inbox.length + outbox.length
  }
}

object Queue {
  def empty: Queue = 
  {
    new Queue(List.empty, List.empty)
  }

  def apply(xs: Seq[Int]): Queue = new Queue(xs.toList, List.empty)
}
