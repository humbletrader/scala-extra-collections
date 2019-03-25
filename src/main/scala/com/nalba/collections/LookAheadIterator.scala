package com.nalba.collections

import scala.collection.AbstractIterator
import scala.util.control.NonFatal

class LookAheadIterator[+A](iter : Iterator[A]) extends AbstractIterator[A] {

  private val bufferedIter  : BufferedIterator[A] = iter.buffered

  override def hasNext: Boolean =
    try{
      if(bufferedIter.hasNext){
            bufferedIter.head //evaluate the head and trigger potential exceptions
            true
      }else{
          false
      }
    }catch{
      case NonFatal(e) =>
        println("caught exception ahead of time")
        false
    }


  override def next() : A = bufferedIter.next()
}
