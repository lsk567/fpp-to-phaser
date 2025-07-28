package fpp.compiler.analysis

import fpp.compiler.util._

type Schedule = List[List[Dag.TaskNode]]

object Scheduler {

    def schedule(dag: Dag, n: Int, a: PhaserAnalysis): Result.Result[Schedule] = {
        val schedInit = List.fill(n)(List.empty[Dag.TaskNode])
        val makespanInit: List[Time] = List.fill(n)(ZERO)
        for {
            sort: List[Dag.Node] <- dag.randomTopologicalSort
            _ <- {
                println("topological sort:")
                println(sort)
                Right(())
            }
            sched <- {
                // Keep track of the makespans of all partitions.
                // Assign task to a partition with the min makespan.
                val (reversedSchedule, _) = sort.foldLeft((schedInit, makespanInit)) {
                    case ((sched, makespan), node: Dag.TaskNode) => 
                        // Find partition with the least amount of work.
                        // FIXME: Perhaps also round-robin to distribute tasks
                        // so that not all ZERO execution time tasks are
                        // assigned to the same partition.
                        val index = makespan.indexOf(makespan.min)
                        // Update the makespan.
                        val startTime = Ordering[Time].max(node.time, makespan(index))
                        // Get the execution time of task. If none is given,
                        // assume it is 0.
                        val execTime = a.deadlineMap.get(node.endpoint.port.toString) match {
                            case Some(t) => t
                            case None => ZERO // FIXME: Check if this would result in really bad schedule.
                        }
                        val makespanUpdated = makespan.updated(index, startTime + execTime)
                        // Prepend the new task node to the partition.
                        val schedUpdated = sched.updated(index, node :: sched(index))
                        (schedUpdated, makespanUpdated)

                    // Skip auxiliary nodes.
                    case ((sched, makespan), _) => (sched, makespan)
                }
                // Reverse the schedule.
                val schedule = reversedSchedule.map(li => li.reverse)
                Right(schedule)
            }
        } yield sched
    }
}
