package fpp.compiler.analysis

import fpp.compiler.ast._
import fpp.compiler.ast.Ast.SpecConnectionGraph
import fpp.compiler.util._

object AnnotationCollector extends AstStateVisitor {

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

  override def defComponentInstanceAnnotatedNode(
    s: State, aNode: Ast.Annotated[AstNode[Ast.DefComponentInstance]]
  ) = {
    val node = aNode._2
    val postNotations = aNode._3
    for {
      s <- postNotations.foldLeft[Result.Result[State]](Right(s)) {
        case (acc, str) =>
          acc.flatMap { state =>
            (PeriodParser.parse(str), OffsetParser.parse(str)) match {
              case (Right(period), _) =>
                println(s"Period parsed: $period")
                val periodMap = s.periodMap + (Symbol.ComponentInstance(aNode) -> period)
                Right(state.copy(periodMap = periodMap))
              case (_, Right(offset)) =>
                println(s"Offset parsed: $offset")
                val offsetMap = state.offsetMap + (Symbol.ComponentInstance(aNode) -> offset)
                Right(state.copy(offsetMap = offsetMap))
              case (Left(err1), Left(err2)) =>
                println(s"Failed to parse: $str")
                println(s"Errors: $err1 | $err2")
                Right(state)
            }
          }
      }
    } yield s
  }

  override def defTopologyAnnotatedNode(
    s: State, aNode: Ast.Annotated[AstNode[Ast.DefTopology]]
  ) = {
    val node = aNode._2
    val data = node.data
    visitList(s, data.members, matchTopologyMember)
  }

  override def specConnectionGraphAnnotatedNode(
    s: State, aNode: Ast.Annotated[AstNode[Ast.SpecConnectionGraph]]
  ) = {
    val preAnnotations = aNode._1
    val node = aNode._2
    val data = node.data
    for {
      s <- {
        if (preAnnotations.length > 0) {
          println("Connection annotation found: " + preAnnotations)
        }
        // Keep the successfully parsed annotations
        // and collect the deadline values in a list.
        val deadlines = preAnnotations
                        .map(DeadlineParser.parse)
                        .collect { case Right(deadline) => deadline }
        // Populate the deadline map.
        Right {
          deadlines.foldLeft (s) { (s, deadline) => 
            s.copy(deadlineMap = s.deadlineMap + (deadline._1 -> deadline._2))
          }
        }
      }
    } yield s
  }

}