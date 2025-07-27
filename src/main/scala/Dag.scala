package fpp.compiler.analysis

import fpp.compiler.analysis.Hyperperiod.PortCall

/**
  * A Dag structure representing task dependencies
  * within a hyperperiod.
  */
class Dag(val downstream: Map[Dag.Node, Set[Dag.Node]]) {

  /** All nodes in the graph */
  def nodes: Set[Dag.Node] = downstream.keySet ++ downstream.values.flatten

}

object Dag {

  /** Base trait for DAG nodes */
  sealed trait Node

  /** Task node represents a port call */
  case class TaskNode(endpoint: Connection.Endpoint, time: Time) extends Node

  /** Time node to mark release or finish time */
  case class TimeNode(time: Time) extends Node

  /** Dummy node to represent a time interval between time nodes */
  case class DummyNode(label: String) extends Node

  /**
    * Construct a Dag from a hyperperiod trace.
    *
    * @param hp The full trace.
    * @param idx The starting index of the repeating hyperperiod.
    */
//   def fromHyperperiod(hp: List[Hyperperiod.Step], idx: Int): Dag = {
    
//     // Drop the first Step of the trace, since it is an initialization step.

//     // For each step in the trace:
//     // - create a time node.
//     // - from the list of port calls, extract the set of rate groups (call._1).
//     //   FIXME: Hyperperiod checking may not involve ports.
//     //   Checking rate group invocations should be enough.
//     // - for each rate group in the set, look up the _list_ of downstream ports
//     //   from PhaserAnalysis.taskMap.
//     // - for each port in the list, create a task node. Connect the time node
//     //   to the head task node, and the connect the rest of the task nodes
//     //   in the list order.

//     // Then create a dummy node for each pair of adjacent time nodes, and set
//     // its execution time to the time difference between the time nodes.
//     // Connect the prior time node to the dummy node, then connect the dummy
//     // node to the later time node.
//   }
}
