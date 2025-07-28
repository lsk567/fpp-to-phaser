package fpp.compiler.analysis

import scala.collection.mutable
import scala.util.Random

import fpp.compiler.util._

/**
  * A Dag structure representing task dependencies
  * within a hyperperiod.
  */
class Dag(val edges: Map[Dag.Node, Set[Dag.Node]]) {

  /** All nodes in the graph */
  def nodes: Set[Dag.Node] = edges.keySet ++ edges.values.flatten

  /**
    * If the DAG is valid, return a random valid topological sort.
    * Randomness is used here so that when a schedule is found invalid,
    * we can generate another schedule from a different topological sort
    * using list scheduling.
    */
  def randomTopologicalSort: Result.Result[List[Dag.Node]] = {
    val inDegree = mutable.Map.empty[Dag.Node, Int].withDefaultValue(0)
    val allNodes = edges.keySet ++ edges.values.flatten

    // Compute in-degrees
    for {
      (from, tos) <- edges
      to <- tos
    } {
      inDegree(to) += 1
    }

    // Initialize S = nodes with zero in-degree
    val zeroIn = mutable.Set.empty[Dag.Node] ++ allNodes.filter(n => inDegree(n) == 0)
    val result = mutable.ListBuffer.empty[Dag.Node]

    while (zeroIn.nonEmpty) {
      val n = Random.shuffle(zeroIn.toList).head
      zeroIn -= n
      result += n

      for (m <- edges.getOrElse(n, Set())) {
        inDegree(m) -= 1
        if (inDegree(m) == 0) zeroIn += m
      }
    }

    // Check for cycle
    if (result.size != allNodes.size)
      throw InternalError("Graph has at least one cycle; topological sort not possible.")
    else
      Right(result.toList)
  }

  /** Visualize the DAG using DOT format */
  def toDot: String = {
    val sb = new StringBuilder
    sb.append("digraph DAG {\n")

    // Assign unique names to nodes
    val nodeIds = nodes.zipWithIndex.toMap

    // Emit nodes
    for (node <- nodes) {
      val id = nodeIds(node)
      val label = node match {
        case Dag.TaskNode(endpoint, time) =>
          s"${endpoint.port} @ ${time.toNanoseconds}ns"
        case Dag.TimeNode(time) =>
          s"time ${time.toNanoseconds}ns"
        case Dag.DummyNode(from, to) =>
          val duration = to - from
          s"Δ${duration.toNanoseconds}ns (${from.toNanoseconds}→${to.toNanoseconds})"
      }
      sb.append(s"""  n$id [label="$label", shape=box];\n""")
    }

    // Emit a subgraph for horizontal ranking of TimeNode and DummyNode
    val timelineNodes = nodes.collect {
      case t: Dag.TimeNode => nodeIds(t)
      case d: Dag.DummyNode => nodeIds(d)
    }

    if (timelineNodes.nonEmpty) {
      sb.append("  { rank=same; ")
      timelineNodes.toList.sorted.foreach(id => sb.append(s"n$id; "))
      sb.append("}\n")
    }

    // Emit edges
    for ((from, tos) <- edges; to <- tos) {
      val fromId = nodeIds(from)
      val toId = nodeIds(to)
      sb.append(s"  n$fromId -> n$toId;\n")
    }

    sb.append("}\n")
    sb.toString
  }

}

object Dag {

  /** Base trait for DAG nodes */
  sealed trait Node

  /** Task node represents a port call invoked at a time point */
  case class TaskNode(endpoint: Connection.Endpoint, time: Time) extends Node

  /** Time node to mark release or finish time */
  case class TimeNode(time: Time) extends Node

  /** Dummy node to represent a time interval between time nodes */
  case class DummyNode(from: Time, to: Time) extends Node

  /**
    * Construct a Dag from a hyperperiod trace.
    *
    * @param hp The full trace.
    * @param idx The starting index of the repeating hyperperiod.
    */
  def fromHyperperiod(
    analysis: PhaserAnalysis,
    hp: List[Hyperperiod.Step],
    idx: Int
  ): Dag = {

    // FIXME: Check for idx != 1
    // Currently we assume that there is no initialization phase.

    // Drop the first init step.
    val steps = hp.drop(1)

    // Mutable map to collect edges: Node -> Set[Node]
    val edges = scala.collection.mutable.Map.empty[Dag.Node, scala.collection.mutable.Set[Dag.Node]]

    // Mutable variable to track previous time node
    var prevTimeNode: Option[Dag.TimeNode] = None

    // Track the last task node seen for each rate group
    val lastTaskByRateGroup = scala.collection.mutable.Map.empty[Symbol.ComponentInstance, Dag.TaskNode]

    for (((time, calls, _), stepIdx) <- steps.zipWithIndex) {
      // Create a time node for this step
      val timeNode = Dag.TimeNode(time)

      // Extract rate groups from the port calls
      val rateGroups = calls.map(_._1)

      if (stepIdx != steps.length - 1) {
        for (rg <- rateGroups) {
          // Lookup task list for this rate group
          val tasks = analysis.taskMap.getOrElse(rg, Nil)

          // Create task nodes for each endpoint
          val taskNodes = tasks.map { case (ep, _) => Dag.TaskNode(ep, time) }

          // Connect timeNode to the first task node, if any
          taskNodes.headOption.foreach { head =>
            edges.getOrElseUpdate(timeNode, scala.collection.mutable.Set.empty) += head
          }

          // Connect taskNodes in sequence: t1 -> t2 -> ...
          for (i <- 0 until taskNodes.length - 1) {
            val from = taskNodes(i)
            val to = taskNodes(i + 1)
            edges.getOrElseUpdate(from, scala.collection.mutable.Set.empty) += to
          }

          // Add deadline edge: last task node of previous firing → this time node
          lastTaskByRateGroup.get(rg).foreach { prevTask =>
            edges.getOrElseUpdate(prevTask, scala.collection.mutable.Set.empty) += timeNode
          }

          // Update last task for this rate group
          taskNodes.lastOption.foreach(lastTask => lastTaskByRateGroup(rg) = lastTask)
        }
      }

      // Create dummy node between previous and current time nodes
      prevTimeNode.foreach { prev =>
        val dummy = Dag.DummyNode(prev.time, time)

        // Do not reuse dummy nodes — always insert new one per pair
        edges.getOrElseUpdate(prev, scala.collection.mutable.Set.empty) += dummy
        edges.getOrElseUpdate(dummy, scala.collection.mutable.Set.empty) += timeNode
      }

      // Update previous time node
      prevTimeNode = Some(timeNode)
    }

    // Patch in deadline edges for any remaining tasks
    prevTimeNode.foreach { lastTimeNode =>
      for ((_, taskNode) <- lastTaskByRateGroup) {
        val existingEdges = edges.getOrElse(taskNode, scala.collection.mutable.Set.empty)
        if (!existingEdges.contains(lastTimeNode)) {
          existingEdges += lastTimeNode
          edges.update(taskNode, existingEdges)
        }
      }
    }

    // Convert to immutable Map[Node, Set[Node]] for the final DAG
    val immutableEdges = edges.view.mapValues(_.toSet).toMap

    new Dag(immutableEdges)
  }
}

