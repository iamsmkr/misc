package com.iamsmkr.misc

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
    val r = vc.filter(_ > 0).map(c => pw.filter(_ <= c).sorted.reverse)

    var shipped = Array[Int]()
    var trips = 0

    while ((pw.filter(_ <= vc.max) diff shipped).nonEmpty) {
      r.foreach { arr =>
        val pending = arr diff shipped
        if (pending.nonEmpty) shipped = shipped :+ pending.head
      }

      trips += 1
    }

    val returnTrips = if (trips > 1) trips - 1 else 0
    trips + returnTrips
  }

  assert(findMinVehicleTime(Array(10, 2, 16, 19), Array(29, 5)) == 5)
  assert(findMinVehicleTime(Array(10, 2, 16, 19), Array(29, 25)) == 3)
  assert(findMinVehicleTime(Array(10, 2, 16, 19), Array(29, 25, 20, 5)) == 1)
}
