package u05lab.code

import java.util.concurrent.TimeUnit

import u05lab.code.PerformanceUtils.measure
import u05lab.code.SeqUtils.{immutableDelete, immutableInsert, middleElement, range}

import scala.collection.immutable.{List, Vector, Set => ImmutableSet, Map => ImmutableMap}
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Set => MutableSet, Map => MutableMap}
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils {
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] {
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
  }

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] = {
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime()-startTime, TimeUnit.NANOSECONDS)
    if(!msg.isEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)
  }

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)
}

object SeqUtils {
  val range: Seq[Int] = 1 to 200000
  val middleElement = 100000

  def immutableInsert(seq: Seq[_])(index: Int): Seq[_] ={
    val (front, back) = seq.splitAt(index)
    front ++ List(-1) ++ back
  }

  def immutableDelete(seq: Seq[_])(index: Int): Seq[_] = {
    val (front, back) = seq.splitAt(index)
    front ++ (back drop 1)
  }
}

object TestLinearSequences {

  def testLinearSequences() {

    /* Linear sequences: List, ListBuffer */
    val immutableList = List(range.toVector)
    val mutableList = ListBuffer.from(range)

    //creating lists
    println("---Lists creation---")
    measure("Immutable list creation")(List(range.toVector))
    measure("Mutable list buffer creation")(ListBuffer.from(range))

    //read lists
    println("")
    println("---Lists read---")
    measure("Immutable list read head")(immutableList.head)
    measure("Mutable list buffer read head")(mutableList.head)

    measure("Immutable list read last element")(immutableList.tail)
    measure("Mutable list buffer read last element")(mutableList.tail)

    //update lists
    println("")
    println("---Lists update---")
    measure("Immutable list update at the end")(immutableList :+ (range.max + 1))
    measure("Mutable list buffer update at the end")(mutableList :+ (range.max + 1))

    measure("Immutable list updated in the middle")(immutableInsert(immutableList)(middleElement))
    measure("Mutable list buffer updated in the middle")(mutableList.insert(middleElement, -1))

    //delete element in lists
    println("")
    println("---Lists delete element---")
    measure("Immutable list delete element in the middle")(immutableDelete(immutableList)(middleElement))
    measure("Mutable list buffer delete element in the middle")(mutableList.remove(middleElement))

    println("")
  }
}

object TestIndexedSequences {

  def testIndexedSequences() {
    /* Indexed sequences: Vector, Array, ArrayBuffer */
    val immutableVector: Vector[Int] = Vector.from(range)
    val mutableArray: ArrayBuffer[Int] = ArrayBuffer.from(range)
    val semiMutableArray: Array[Int] = Array.from(range)

    //creating indexed sequences
    println("---Indexed sequences creation---")
    measure("Immutable vector creation")(Vector.from(range))
    measure("Mutable array buffer creation")(ArrayBuffer.from(range))
    measure("Semi mutable array creation")(Array.from(range))


    //read indexed sequences
    println("")
    println("---Indexed sequences read---")
    measure("Immutable vector read head")(immutableVector.head)
    measure("Mutable array buffer read head")(mutableArray.head)
    measure("Semi mutable array read head")(semiMutableArray.head)

    measure("Immutable vector read last element")(immutableVector.tail)
    measure("Mutable array buffer read last element")(mutableArray.tail)
    measure("Semi mutable array read last element")(semiMutableArray.tail)

    //update indexed sequences
    println("")
    println("---Indexed sequences update---")
    measure("Immutable vector update at the end")(immutableVector :+ (range.max + 1))
    measure("Mutable array buffer update at the end")(mutableArray :+ (range.max + 1))
    measure("Semi mutable array update at the end")(semiMutableArray :+ (range.max + 1))

    measure("Immutable vector update in the middle")(immutableInsert(immutableVector)(middleElement))
    measure("Mutable array buffer update in the middle")(mutableArray.insert(middleElement, -1))
    measure("Semi mutable array update in the middle")(immutableInsert(semiMutableArray)(middleElement))

    //delete element in indexed sequences
    println("")
    println("---Indexed sequences delete element---")
    measure("Immutable vector delete element in the middle")(immutableDelete(immutableVector)(middleElement))
    measure("Mutable array buffer delete element in the middle")(mutableArray.remove(middleElement))
    measure("Semi mutable array delete element in the middle")(immutableDelete(semiMutableArray)(middleElement))

    println("")
  }
}

object TestSets{
  def testSets() {
    val immutableSet: ImmutableSet[Int] = ImmutableSet.from(range)
    val mutableSet: MutableSet[Int] = MutableSet.from(range)

    //creating sets
    println("---Sets creation---")
    measure("Immutable set creation")(ImmutableSet.from(range))
    measure("Mutable set creation")(MutableSet.from(range))

    //read sets
    println("")
    println("---Sets read---")
    measure("Immutable set read head")(immutableSet.head)
    measure("Mutable set read head")(mutableSet.head)

    measure("Immutable set read last element")(immutableSet.tail)
    measure("Mutable set read last element")(mutableSet.tail)

    //update sets
    println("")
    println("---Sets update---")
    measure("Immutable set update")(immutableSet + (range.max + 1))
    measure("Mutable set update")(mutableSet + (range.max + 1))

    //delete element in sets
    println("")
    println("---Sets delete element---")
    measure("Immutable set delete element in the middle")(immutableSet - middleElement)
    measure("Mutable set delete element in the middle")(mutableSet.remove(middleElement))

    println("")
  }
}

object TestMaps {
  def testMaps() {

    val immutableMap: ImmutableMap[Int,Int] = ImmutableMap.from(range.map(e => (e,e)).toMap)
    val mutableMap = MutableMap.from(range.map(e => (e,e)).toMap)

    //creating maps
    println("---Maps creation---")
    measure("Immutable map creation")(ImmutableMap.from(range.map(e => (e,e)).toMap))
    measure("Mutable map creation")(MutableMap.from(range.map(e => (e,e)).toMap))

    //read maps
    println("")
    println("---Maps read---")
    measure("Immutable map read head")(immutableMap.head)
    measure("Mutable map read head")(mutableMap.head)

    measure("Immutable map read last element")(immutableMap.tail)
    measure("Mutable map read last element")(mutableMap.tail)

    //update maps
    println("")
    println("---Maps update---")
    measure("Immutable map update")(immutableMap + ((range.max + 1) -> (range.max + 1)))
    measure("Mutable map update")(mutableMap.addOne((range.max + 1) -> (range.max + 1)))

    //delete element in maps
    println("")
    println("---Maps delete element---")
    measure("Immutable map delete element in the middle")(immutableMap - middleElement)
    measure("Mutable map delete element in the middle")(mutableMap.remove(middleElement))

    println("")
  }
}


object CollectionsTest extends App {
  import u05lab.code.TestLinearSequences.testLinearSequences
  testLinearSequences()

  import u05lab.code.TestIndexedSequences.testIndexedSequences
  testIndexedSequences()

  import u05lab.code.TestSets.testSets
  testSets()

  import u05lab.code.TestMaps.testMaps
  testMaps()

}
/* Immutable collections are overall more efficients than the mutable ones. The update is  a little bit
* less efficient, but it is anyway a good tradeoff apparently*/