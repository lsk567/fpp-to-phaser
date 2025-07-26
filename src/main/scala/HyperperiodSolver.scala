package fpp.compiler.analysis

import fpp.compiler.ast._

object HyperperiodSolver {


    // Timestamp, current set of invocations, a map of future timestamps to future invocations. 
    type Step = (Time, Set[PortInstanceIdentifier], Map[Time, Set[PortInstanceIdentifier]])
    
    /**
      * To compute the next step, take the earliest events from
      * the pending events list, set their 
      *
      * @return
      */
    // def next(s: Step): Step = {

    // }

    // hp = hyperperiod
    @annotation.tailrec
    def solve(
        current: Step,
        trace: List[Step]
    )(
        next: Step => Step,
        hpCheck: (Step, List[Step]) => Option[Int]
    ): (List[Step], Int) = {
        hpCheck(current, trace) match {
            case Some(index) =>
                val fullTrace = (current :: trace).reverse
                (fullTrace, index)
            case None =>
                solve(next(current), current :: trace)(next, hpCheck)
        }
    }
}