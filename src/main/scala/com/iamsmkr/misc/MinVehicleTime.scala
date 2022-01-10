package com.iamsmkr.misc

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Input:
 * package_weight array
 * vehicle_capacity array
 *
 * Problem:
 * Find "Minimum Time" required in hours to ship all packages from source to destination.
 * Return "not possible" in case packages cannot be moved.
 *
 * Constraints:
 * 1. Vehicle can carry only one package at a time
 * 2. Vehicle cannot carry a package more than its capacity
 * 3. Source to Destination: 1 hour
 * 4. Destination to Source: 1 hour
 *
 * Example:
 * package_weight = [10, 2, 16, 19]
 * vehicle_capacity = [29, 5]
 * Output: 5
 *
 * package_weight = [10, 2, 16, 19]
 * vehicle_capacity = [29, 25]
 * Output: 3
 *
 * package_weight = [10, 2, 16, 19]
 * vehicle_capacity = [29, 25, 20, 5]
 * Output: 1
 */

/**
 * Solution: O(N)
 *
 * This solution could be better understood with a different input set.
 * package_weight = [10, 2, 16, 19, 21, 26, 24, 3]
 * vehicle_capacity = [20, 25, 5, 29]
 *
 * We can re-represent the input set as following:
 * 5  -> 3, 2
 * 20 -> 19, 16, 10, 3, 2
 * 25 -> 24, 21, 19, 16, 10, 3, 2
 * 29 -> 26, 24, 21, 19, 16, 10, 3, 2
 *
 * This re-representation lists all package weights that a vehicle can carry based on its capacity.
 *
 * Using this re-representation, in first iteration we can ship (3, 19, 24, 26). Second iteration (2, 16, 21, 10).
 * All we have to do is to ensure is that we consider shipped items in every iteration.
 */

object MinVehicleTime extends App {

  def findMinVehicleTime(pw: Array[Int], vc: Array[Int]): Int = {

    // This map is used to eliminate duplicate Arrays that are more than the number of elements in a particular Array.
    // For instance, Array(Array(2, 2), Array(2, 2)) should result in Array(Array(2, 2), Array(2, 2)) but
    // Array(Array(2, 2), Array(2, 2), Array(2, 2)) should also result in Array(Array(2, 2), Array(2, 2)).
    val m: mutable.Map[String, (Int, Int)] = mutable.Map()

    val r = vc.filter(_ > 0).map { c =>
      val p = ArrayBuffer(pw.filter(_ > 0).filter(_ <= c).sorted.reverse: _*)
      val str = p.mkString("")
      if (m.isDefinedAt(str)) {
        if (m(str)._2 == m(str)._1) ArrayBuffer[Int]()
        else {
          m.put(str, (m(str)._1 + 1, m(str)._2))
          p
        }
      } else {
        m.put(str, (1, p.length))
        p
      }
    }.filter(_.nonEmpty)

    var shipped = Array[Int]()
    var shippedPerIter = Array[Int]()
    var trips = 0

    var counter = 0

    val shippable = pw.filter(_ <= vc.max)

    while ((shippable diff shipped).nonEmpty) {
      val k = r.map(_.--=(shippedPerIter)).filter(_.nonEmpty)

      shippedPerIter = Array[Int]()

      k.foreach { arr =>
        counter = counter + 1
        val pending = arr diff shippedPerIter
        if (pending.nonEmpty) {
          shipped = shipped :+ pending.head
          shippedPerIter = shippedPerIter :+ pending.head
        }
      }

      trips += 1
    }

    println(counter)

    val returnTrips = if (trips > 1) trips - 1 else 0
    trips + returnTrips
  }

    assert(findMinVehicleTime(Array(10, 9, 8, 7, 6, 5, 4, 3, 2, 1), Array(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)) == 1)
    assert(findMinVehicleTime(Array(10, 9, 8, 7, 6, 5, 4, 3, 2, 1), Array(10, 2, 1)) == 15)
    assert(findMinVehicleTime(Array(10, 9, 8, 7, 6, 5, 4, 3, 2, 1), Array(11, 1)) == 17)
    assert(findMinVehicleTime(Array(19, 2, 2, 2, 2), Array(29, 3, 3, 3, 3)) == 1)
    assert(findMinVehicleTime(Array(10, 2, 16, 19), Array(29, 3, 3, 3, 3, 3, 3, 3)) == 5)
    assert(findMinVehicleTime(Array(10, 10, 10, 10), Array(100, 100)) == 3)
    assert(findMinVehicleTime(Array(10, 10, 10, 10), Array(100)) == 7)
    assert(findMinVehicleTime(Array(10, 2, 16, 19), Array(29, 5)) == 5)
    assert(findMinVehicleTime(Array(10, 2, 16, 19), Array(29, 25)) == 3)
    assert(findMinVehicleTime(Array(10, 2, 16, 19), Array(29, 25, 20, 5)) == 1)
}
