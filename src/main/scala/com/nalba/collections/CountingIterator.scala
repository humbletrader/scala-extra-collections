package com.nalba.collections

class CountingIterator[+A](iter: Iterator[A]) extends Iterator[A]{

  private var consumedCounter = 0

  def next() : A = {
    consumedCounter += 1
    iter.next()
  }

  def hasNext() : Boolean = iter.hasNext

  def consumed() : Int = consumedCounter


}
