package com.nalba.collections

import scala.collection.mutable

object HashBasedTable{

  def empty[R, C, V] = new HashBasedTable(Map.empty[R, Map[C, V]])

}


class HashBasedTable[R, C, V](backingMap : Map[R, Map[C, V]]) extends Table[R, C, V]{

  def get(r: R, c: C) : Option[V] = backingMap.get(r).flatMap(_.get(c))

  def apply(r: R, c: C) : V = backingMap(r)(c)

  def row(r: R) : Option[Map[C, V]] = backingMap.get(r)

  def apply(r: R) = backingMap(r)

  def updated(r: R, c: C, v: V) = new HashBasedTable({
    val rowValues = backingMap.getOrElse(r, Map.empty[C, V])
    backingMap.updated(r, rowValues.updated(c, v))
  })

  def map[That](f : (R,C,V) => That) : HashBasedTable[R, C, That] =
    new HashBasedTable(
      backingMap.map{ case (rowKey, columnValues) =>
        rowKey -> columnValues.map{ case (colKey, value) =>
          colKey -> f(rowKey, colKey, value)
        }
      }
    )

  def size : Int = backingMap.values.foldLeft(0)((agg, rowValues) => agg + rowValues.size)

  override def toString: String = backingMap.toString()

  //todo: equals and hashCode

}

class HashTableBuilder[R, C, V]{

  private val mapOfRowBuilders = new mutable.HashMap[R, mutable.MapBuilder[C, V, mutable.Map[C, V]]]()

  def +=(r: R, c: C, v: V) : HashTableBuilder[R, C, V] = {
    val rowBuilder = mapOfRowBuilders.getOrElseUpdate(r,
      new mutable.MapBuilder[C, V, mutable.Map[C, V]](mutable.Map.empty[C, V])
    )
    rowBuilder += c -> v

    this
  }

  def result() : Table[R, C, V] = new HashBasedTable(
    mapOfRowBuilders.map{case (rowKey, rowBuilder) =>
      rowKey -> rowBuilder.result().toMap
    }.toMap
  )

}
