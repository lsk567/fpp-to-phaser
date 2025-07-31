package fpp.compiler.analysis

import fpp.compiler.util._

type Schedule = List[List[Dag.TaskNode]]

object Scheduler {

    /**
      * Perform a topological sort on the input unpartitioned DAG.
      * Each node in the sort is assigned to the partition with
      * the least amount of work.
      * 
      * Since phasers currently do not have synchronization mechanism
      * among themselves, we cannot distribute port calls from one
      * rate group to two phasers. The opportunity for parallelism
      * is reduced.
      *
      * @param dag Input DAG to be partitioned / scheduled
      * @param n Number of partitions
      * @param a Phaser analysis object
      * @param mapEntireRateGroup Whether an entire rate group should
      * be mapped to a partition. This is set to true by default to
      * prevent the need for inter-phaser synchronization. It can be
      * set to false when phasers coordinate with start time and
      * components do not have shared memory. E.g., the downstream
      * phaser triggers a queued component. If the upstream task
      * overruns and the downstream task executes before the upstream
      * finishes, there could be memory corruption.
      */
    def schedule(
        dag: Dag, 
        n: Int, 
        a: PhaserAnalysis, 
        mapEntireRateGroup: Boolean = true
    ): Result.Result[Schedule] = {
        val schedInit = List.fill(n)(List.empty[Dag.TaskNode])
        val makespanInit: List[Time] = List.fill(n)(ZERO)
        
        //// Data structures required for mapEntireRateGroup
        // A list of total RG makespan.
        val rateGroups = a.taskMap.keys.toList
        val rateGroupMakespan: Map[Symbol.ComponentInstance, Time] =
            rateGroups.foldLeft(Map[Symbol.ComponentInstance, Time]()){
                (map, rg) => {
                    // Sum up all execution times of ports called by the rate group.
                    val makespan: Time = a.taskMap(rg).foldLeft(ZERO: Time){
                        case (ms, (task, _)) => {
                            val execTime = a.deadlineMap.get(task.port.toString) match {
                                case Some(t) => t
                                case None => ZERO
                            }
                            ms + execTime
                        }
                    }
                    map + (rg -> makespan)
                }
            }
        // A running map from partition to rate group
        val rateGroupToPartitionInit: Map[Symbol.ComponentInstance, Int] = Map()
        // and a way to check whether a node belongs to a rate group.
        val endpointToInstance: Map[Connection.Endpoint, Symbol.ComponentInstance] =
            a.taskMap.flatMap { case (rg, list) => list.map { case (ep, _) => ep -> rg } }

        for {
            // Generate a topological sort.
            sort: List[Dag.Node] <- dag.randomTopologicalSort
            // Run list scheduling algorithm.
            pair <- {
                // Keep track of the makespans of all partitions.
                // Assign task to a partition with the min makespan.
                val (reversedSchedule, fullMakespan, _) = sort.foldLeft(
                    (schedInit, makespanInit, rateGroupToPartitionInit)) {
                    case ((sched, makespan, rg2part), node: Dag.TaskNode) => 
                        // Check which rate group this task belongs to,
                        // only needed when mapEntireRateGroup.
                        val rg = endpointToInstance(node.task._1)
                        // Choose a partition to assign the task to.
                        val index =
                            if (mapEntireRateGroup) then {
                                // Check if this rate group is already assigned.
                                rg2part.get(rg) match {
                                    // If so, set index to the assigned partition.
                                    case Some(idx) => idx
                                    // Otherwise, choose the partition with the least work.
                                    case None => makespan.indexOf(makespan.min)
                                }
                            } else {
                                // Find partition with the least amount of work.
                                // FIXME: Perhaps also round-robin to distribute tasks
                                // so that not all ZERO execution time tasks are
                                // assigned to the same partition.
                                makespan.indexOf(makespan.min)
                            }
                        // Update the makespan.
                        val startTime = Ordering[Time].max(node.time, makespan(index))
                        // Get the execution time of task. If none is given,
                        // assume it is 0.
                        val execTime = a.deadlineMap.get(node.task._1.port.toString) match {
                            case Some(t) => t
                            case None => ZERO // FIXME: Check if this would result in really bad schedule.
                        }
                        // Prepend the new task node to the partition.
                        val schedUpdated = sched.updated(index, node :: sched(index))
                        val makespanUpdated = makespan.updated(index, startTime + execTime)
                        val rg2partUpdated = rg2part.updated(rg, index)
                        (schedUpdated, makespanUpdated, rg2partUpdated)

                    // Skip auxiliary nodes.
                    case ((sched, makespan, rg2part), _) => (sched, makespan, rg2part)
                }
                // Since nodes are prepended, reverse the list
                // to get the correct order.
                val schedule = reversedSchedule.map(li => li.reverse)
                Right((schedule, fullMakespan))
            }
            (sched, makespan) = pair
            _ <- {
                for ((s, i) <- sched.zipWithIndex) {
                    println(s"Schedule $i:")
                    println(s)
                }
                Right(())
            }
            // Validate makespan.
            // FIXME: This does not check intermediate deadlines from rate group periods.
            _ <- {
                val bound = a.hyperperiod._3
                val violations = makespan.zipWithIndex.filter { case (ms, _) => ms >= bound }

                if (violations.isEmpty) {
                    Right(())
                } else {
                    val details = violations.map { case (ms, idx) =>
                    s"Partition $idx: makespan=$ms (bound=$bound)"
                    }.mkString("\n")

                    throw InternalError(s"The schedule does not fit within bound $bound.\nViolations:\n$details")
                }
            }
        } yield sched
    }

    /**
      * Based on the partitioned schedule, create a map for each phaser
      * that maps a task assigned to the phaser to an output port in
      * the order of the scheduled port calls.
      *
      * @param sched
      */
    def assignPhaserPorts(sched: Schedule): List[Map[Task, Int]] = {
        val list = sched.foldLeft(List.empty[Map[Task, Int]]) {
            (li, partition) => {
                // Follow the schedule and map each task to its index in the schedule.
                val phaserPortMap = partition.zipWithIndex.foldLeft(Map.empty[Task, Int]) {
                    case (m, (taskNode, taskIndex)) =>
                        // If the task already has a port from its prior invocation,
                        // do not overwrite it with another taskIndex, which destroys
                        // the linear port assignment in the map.
                        m.get(taskNode.task) match {
                            case Some(idx) => m
                            case None => m + (taskNode.task -> taskIndex)
                        }
                }
                phaserPortMap :: li
            }
        }
        list.reverse
    }
}
