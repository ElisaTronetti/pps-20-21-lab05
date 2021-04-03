package u05lab

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u05lab.code.List
import u05lab.code.sequenceOnList.{sequence, sequence1}

class ListsTest {

  @Test def testZipRight(){
    val l = List(10,20,30,40)
    assertEquals(List((10,0), (20,1), (30,2), (40,3)), l.zipRight)
  }

  @Test def testPartition(){
    val l = List(10,20,30,40)
    assertEquals((List(20,30,40), List(10)), l.partition(_>15))
  }

  @Test def testSpan(){
    val l = List(10,20,30,40)
    assertEquals((List.nil, List(10,20,30,40)), l.span(_>15))
    assertEquals((List(10), List(20,30,40)), l.span(_<15))
  }

  @Test def testReduce(){
    val l = List(10,20,30,40)
    assertEquals(100, l.reduce(_+_))
    try { List[Int]().reduce(_+_); assert(false) } catch { case _:UnsupportedOperationException => }
  }

  @Test def testTakeRight(){
    val l = List(10,20,30,40)
    assertEquals(List(30,40), l.takeRight(2))
  }

  @Test def testCollect(){
    val l = List(10,20,30,40)
    assertEquals(List(9,39), l.collect{case x if x<15 || x>35 => x-1})
  }

  @Test def testSequence(){
    val l: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    assertEquals(Some(List(1,2,3)), sequence(l))

    val l1: List[Option[Int]] = List(Some(1), None, Some(3))
    assertEquals(None, sequence(l1))
  }

  @Test def testSequence1(){
    val l: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    assertEquals(Some(List(1,2,3)), sequence1(l))

    val l1: List[Option[Int]] = List(Some(1), None, Some(3))
    assertEquals(None, sequence1(l1))
  }

}
