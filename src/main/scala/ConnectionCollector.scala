package fpp.compiler.analysis

import fpp.compiler.ast._
import fpp.compiler.ast.Ast.SpecConnectionGraph
import fpp.compiler.util._

object ConnectionCollector extends AstStateVisitor {

  type State = PhaserAnalysis

  override def transUnit(s: State, tu: Ast.TransUnit): Result.Result[State] =
    visitList(s, tu.members, matchTuMember)

  def tuList(s: State, tul: List[Ast.TransUnit]): Result.Result[State] =
    for {
      s <- visitList(s, tul, transUnit)
    } yield s

  override def defModuleAnnotatedNode(
    s: State,
    aNode: Ast.Annotated[AstNode[Ast.DefModule]]
  ) = {
    val node = aNode._2
    val data = node.data
    visitList(s, data.members, matchModuleMember)
  }

  override def defTopologyAnnotatedNode(
    s: State,
    aNode: Ast.Annotated[AstNode[Ast.DefTopology]]
  ) = {
    val node = aNode._2
    val data = node.data

    for {
      // Locate the semantic version of topology.
      symbol <- {
        println(s"Topology found in topology.fpp: ${data.name}")
        Right(Symbol.Topology(aNode))
      }
      topology <- Right(s.analysis.topologyMap(symbol))
      // Find all connections from rate groups.
      rateGroupConnections <- Right {
        topology.connectionMap.values.flatten.toList.filter(
          conn => {
            val fromPort: PortInstanceIdentifier = conn.from.port
            val fromPortInstance: PortInstance = fromPort.portInstance
            fromPortInstance.match {
              case PortInstance.General(_, _, _, _, ty, _) => {
                val compInstance = fromPort.componentInstance
                val comp = compInstance.component
                val compName = comp.aNode._2.data.name
                // A connection passes the filter if
                // the upstream component is an active rate group
                // and the from port has type Sched. 
                compName == "ActiveRateGroup"
                  && ty.toString() == "Sched"
              }
              case _ => false
            }
          }
        )
      }
      // Map rate groups to destination ports.
      s <- {
        // For each connection, group connections by rate group instances.
        // Then sort the resulting list by portNumber.
        val updatedTaskMap = rateGroupConnections.map { conn =>
          val rateGroupInstance = conn.from.port.componentInstance.aNode
          val toEndpoint = conn.to
          val fromPortNumber = conn.from.portNumber match {
            case Some(i) => i
            case None => 0 // FIXME: This does not account for auto-assigning port numbers.
          }
          // Map each connection to a key-value tuple.
          (Symbol.ComponentInstance(rateGroupInstance), (toEndpoint, fromPortNumber))
        }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2).sortBy(_._2))
        .toMap

        println("taskMap:")
        println(updatedTaskMap)

        // Return a Result monad with an updated state.
        Right(s.copy(taskMap = updatedTaskMap))
      }
      s <- visitList(s, data.members, matchTopologyMember)
    }
    yield s
  }

}