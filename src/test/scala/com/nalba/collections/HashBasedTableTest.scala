package com.nalba.collections

import org.scalatest.FunSuite

class HashBasedTableTest extends FunSuite{

  test("table.get returns None when row and or column key does not exist"){
    val classUnderTest = new HashBasedTable[String, String, Int](
      Map[String, Map[String, Int]](
        "row 1" -> Map("col 1" -> 1, "col 2" -> 10, "col 3" -> 100),
        "row 2" -> Map("column 1" -> 2, "column 2" -> 20, "column 3" -> 200)
      )
    )

    assert(None === classUnderTest.get("inexistent", "col 1"))
    assert(None === classUnderTest.get("row 1", "inexistent col"))
  }


  test("Table.map changes all values in the table"){
    val classUnderTest = new HashBasedTable[String, String, Int](
      Map[String, Map[String, Int]](
        "row 1" -> Map("col 1" -> 1, "col 2" -> 10, "col 3" -> 100),
        "row 2" -> Map("column 1" -> 2, "column 2" -> 20, "column 3" -> 200)
      )
    )

    val result = classUnderTest.map{case (r,c,v) => v + 1}

    assert(Some(2) === result.get("row 1", "col 1"))
    assert(Some(11) === result.get("row 1", "col 2"))
    assert(Some(101) === result.get("row 1", "col 3"))
  }

  test("HashBasedTable.updated creates another table with an update of the original value"){
    val classUnderTest = new HashBasedTable[String, String, Int](
      Map[String, Map[String, Int]](
        "row 1" -> Map("col 1" -> 1, "col 2" -> 10, "col 3" -> 100),
        "row 2" -> Map("column 1" -> 2, "column 2" -> 20, "column 3" -> 200)
      )
    )

    val updatedTable = classUnderTest.updated("row 1", "col 1", 1000)

    assert(updatedTable.get("row 1", "col 1") === Some(1000))
  }

  test("HashBasedTable.updated creates a new table with non-existent value"){
    val original = new HashBasedTable[String, String, Int](
      Map[String, Map[String, Int]](
        "row 1" -> Map("col 1" -> 1, "col 2" -> 10, "col 3" -> 100),
        "row 2" -> Map("column 1" -> 2, "column 2" -> 20, "column 3" -> 200)
      )
    )

    val underTest = original.updated("row 1000", "col 1000", 1000)

    assert(underTest.get("row 1000", "col 1000") === Some(1000))
    assert(underTest.size === original.size + 1)
  }

  test("HashBasedTable.size counts all objects in the table"){
    val classUnderTest = new HashBasedTable[String, String, Int](
      Map[String, Map[String, Int]](
        "row 0" -> Map.empty[String, Int],            // 0 elements
        "row 1" -> Map("col 1" -> 1, "col 2" -> 10, "col 3" -> 100), //3 elements
        "row 2" -> Map("column 1" -> 2, "column 2" -> 20)             // 2 elements
      )
    )

    assert(3 + 2 === classUnderTest.size)

    assert(0 === new HashBasedTable(Map.empty[Int, Map[Double, String]]).size)
  }

  test("Table.apply builds a HashBasedTable"){
    val classUnderTest = Table[Int, Double, String](
      (1, 1.0, "1"),
      (2, 2.0, "2"),
      (3, 3.0, "3"),
      (2, 20.0, "20")
    )
    assert(Some("1") === classUnderTest.get(1, 1.0))
    assert(Some("3") === classUnderTest.get(3, 3.0))
    assert(Some("2") === classUnderTest.get(2, 2.0))
    assert(Some("20") === classUnderTest.get(2, 20.0))
  }

  test("apply returns an existing value or throws exception"){
    val classUnderTest = Table[String, Int, Double](
      ("one", 1, 1.0),
      ("two", 2, 2.0),
      ("three", 3, 3.0),
      ("one", 10, 10.0)
    )

    assert( 2.0 === classUnderTest("two", 2))
    assert(1.0 === classUnderTest("one", 1))
    assert(10.0 === classUnderTest("one", 10))
    assertThrows[NoSuchElementException](classUnderTest("seven", 1))
    assertThrows[NoSuchElementException](classUnderTest("one", 7))
  }

  test("apply returns a full row or throws exception"){
    val classUnderTest = Table[String, Int, Double](
      ("one", 1, 1.0),
      ("two", 2, 2.0),
      ("three", 3, 3.0),
      ("one", 10, 10.0)
    )

    assert( Map(2 -> 2.0) === classUnderTest("two"))
    assert( Map(1 -> 1.0, 10 -> 10.0) === classUnderTest("one"))
    assert( Map(3 -> 3.0) === classUnderTest("three"))
    assertThrows[NoSuchElementException](classUnderTest("seven"))
  }

}
