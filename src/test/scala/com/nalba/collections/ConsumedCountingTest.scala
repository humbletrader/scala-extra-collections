package com.nalba.collections

import org.scalatest.FunSuite

class ConsumedCountingTest extends FunSuite {

  test("count iterator is correct"){
    val underTest = new CountingIterator(Iterator(100,200,300))

    assert(underTest.consumed() === 0)
    assert(underTest.next() === 100)
    assert(underTest.consumed() === 1)
    assert(underTest.next() === 200)
    assert(underTest.consumed() === 2)
    assert(underTest.next() === 300)
    assert(underTest.consumed() === 3)
    assert(underTest.hasNext() === false)
    assert(underTest.consumed() === 3)
  }

  test("empty counting iterator returns 0"){
    val underTest = new CountingIterator[Int](Iterator())
    assert(underTest.consumed() === 0)
    assert(underTest.hasNext() === false)
    assert(underTest.consumed() === 0)
  }

}
