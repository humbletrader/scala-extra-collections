package com.nalba.collections

object Table{

  def empty[R, C, V] = new HashBasedTable(Map.empty[R, Map[C, V]])

  def apply[R, C, V](seq: (R, C, V)*): Table[R, C, V] = {
    val builder = new HashTableBuilder[R, C, V]
    for((r,c,v) <- seq){
      builder += (r, c, v)
    }

    builder.result()
  }
}

trait Table[R, C, V] {

  def get(r: R, c: C) : Option[V]

  def apply(r: R, c: C) : V

  def row(r: R) : Option[Map[C, V]]

  def apply(r: R) : Map[C, V]

  def updated(r: R, c: C, v: V) : Table[R, C, V]

  def map[That](f : (R,C,V) => That) : Table[R, C, That]

  def size : Int
}