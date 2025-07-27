package fpp.compiler.analysis

import fpp.compiler.ast._

object HyperperiodSolver {

    /** A port call consists of a rate group (caller) and a port instance (callee). */
    type PortCall = (Symbol.ComponentInstance, PortInstanceIdentifier)

    /**
      * A step consists of three elements:
      * 1. a current timestamp,
      * 2. a set of current port calls,
      * 3. a map from future timestamps to future port calls. 
      */
    type Step = (Time, Set[PortCall], Map[Time, Set[PortCall]])

    /**
      * Find initial offsets of all rate groups, and add them
      * to the step's future port call map.
      *
      * @param analysis
      * @return
      */
    def initStep(analysis: PhaserAnalysis): Step = {
        // FIXME: Assume all rate groups have @period labels.
        // This needs to be more robust by having a rate group list
        // in the phaser analysis.
        val rateGroups = analysis.offsetMap.keys
        val offsets = rateGroups.map(rg =>
            (rg, analysis.offsetMap.getOrElse(rg, ZERO)))
        val map = offsets.foldLeft
            (Map[Time, Set[PortCall]]())
            ((m, offset) => {
                val (rg, o) = offset
                // A list of port calls made by the rate group
                val calls: List[PortCall] =
                    analysis.taskMap(rg).map(tup => {
                        val (endpoint, _) = tup
                        (rg, endpoint.port)
                    })
                m.updatedWith(o) {
                    case Some(set) => Some(set.union(calls.toSet))
                    case None => Some(calls.toSet)
                }
            })
        (ZERO, Set(), map)
    }

    @annotation.tailrec
    def solve(
        analysis: PhaserAnalysis,
        current: Step,
        trace: List[Step]
    )(
        next: (PhaserAnalysis, Step) => Step,
        hpCheck: (Step, List[Step]) => Option[Int]
    ): (List[Step], Int) = {
        hpCheck(current, trace) match {
            case Some(index) =>
                val fullTrace = (current :: trace).reverse
                (fullTrace, index)
            case None =>
                solve(analysis, next(analysis, current), current :: trace)(next, hpCheck)
        }
    }

    /**
      * The next step's timestamp and port calls are the earliest entry
      * of the pending events map. Then remove the entry from the map and
      * add new pending events to the map based on the current invocations.  
      */
    def next(a: PhaserAnalysis, step: Step): Step = {
        val (_, _, currentMap) = step
        // Get the next time and port calls.
        val (nextTime, nextCalls): (Time, Set[PortCall]) = currentMap.minBy(_._1)
        // Infer calls beyond the next time.
        val futureCalls: Set[(Time, PortCall)] =
            nextCalls.map(call => {
                // For each call, find the caller rate group's period,
                // add to the current time.
                val (rateGroup, _) = call
                val period = a.periodMap(rateGroup)
                val futureTime = nextTime + period
                (futureTime, call)
            })        
        // Create the next map using future calls.
        val nextMapInit = currentMap.removed(nextTime)
        val nextMap = futureCalls.foldLeft(nextMapInit)((map, timedCall) => {
            val (time, call) = timedCall
            map.updatedWith(time) {
                case Some(set) => Some(set + call)
                case None => Some(Set(call))
            }
        })
        (nextTime, nextCalls, nextMap)
    }

    /**
      * Given a new step and an existing trace, check if the step
      * has occured in the trace before. If so, return the index of recurrence.
      * 
      * A step s matches a prior step s' in the trace when they have the same port calls,
      * and all entries in the map have the same time interval relative to
      * the timestamp of the step.
      * 
      * Specially, denote the current time as t, the previous time as t', the maps m and m'.
      * For an entry e in m, where e = (time -> Set(calls)),
      * we check if there exists an e' = (time' -> Set(calls')) in m',
      * such that calls = calls' and time - t = time' - t'. If this holds for all entries in m,
      * then we say step s and s' "match".
      *
      * @param step
      * @param trace
      */
    def hpCheck(step: Step, trace: List[Step]): Option[Int] = {
        val (t, calls, m) = step

        trace.zipWithIndex.find { case ((tPrev, callsPrev, mPrev), idx) =>
            // First check if the current port calls match the previous step's port calls
            if (calls != callsPrev) false
            else {
                // Normalize both maps by shifting their time keys relative to their respective timestamps
                def normalizeMap(baseTime: Time, map: Map[Time, Set[PortCall]]): Map[BigInt, Set[PortCall]] =
                    map.map { case (time, callSet) => (time.toNanoseconds - baseTime.toNanoseconds) -> callSet }

                val normM     = normalizeMap(t, m)
                val normMPrev = normalizeMap(tPrev, mPrev)

                normM == normMPrev
            }
        }.map(_._2) // Return the index of the matching step
    }
    
    def toDot(hyperperiod: (List[Step], Int)): String = {
        val (trace, recurrenceStart) = hyperperiod
        val sb = new StringBuilder
        sb ++= "digraph Hyperperiod {\n"
        sb ++= "  rankdir=LR;\n"

        // Create nodes
        for ((step, idx) <- trace.zipWithIndex) {
            val (t, calls, _) = step
            val labelCalls = calls.map { case (rg, port) =>
            s"${rg.getUnqualifiedName}→${port.toString}"
            }.mkString("\\n")

            sb ++= s"""  s$idx [label="t=${t.toNanoseconds}ns\\n$labelCalls"];\n"""
        }

        // Create edges
        for (idx <- 0 until trace.length - 1) {
            sb ++= s"  s$idx -> s${idx + 1};\n"
        }

        // Loop back to indicate recurrence
        if (recurrenceStart < trace.length - 1) {
            sb ++= s"""  s${trace.length - 1} -> s$recurrenceStart [style=dashed, label="recurs"];\n"""
        }

        sb ++= "}\n"
        sb.toString
    }

}