package fpp.compiler.codegen

import fpp.compiler.analysis._

case class PhaserConfigCppWriter(
    s: CppWriterState,
    pa: PhaserAnalysis,
) extends CppWriterUtils {

    def write: CppDoc = {
        val description = s"Configuration for active phasers"
        val fileName = s"phaser_config"
        val includeGuard = s""
        CppWriter.createCppDoc(
            description,
            fileName,
            includeGuard,
            getMembers,
            s.toolName
        )
    }

    private def getMembers: List[CppDoc.Member] = {
        List(
            getConstructFn
        )
    }

    private def getConstructFn: CppDoc.Member.Function = {
        val comment = s"Register phases for each partition."
        val name = s"configure_phasers"
        val body = pa.schedule.zipWithIndex.foldLeft(List.empty[Line]) {
            case (li, (partition, pIndex)) => {
                // For each call within a partition,
                // generate a registration.
                val regs = partition.zipWithIndex.foldLeft(List.empty[Line]) {
                    case (l, (taskNode, cIndex)) => {
                        val outputChannel = taskNode.task._2
                        val port = taskNode.task._1.port.toString
                        val execTime = pa.deadlineMap.get(port)
                        val numTicks: String = execTime match {
                            case Some(t) => Time.ratio(t, pa.tick).toString
                            case None => "DONT_CARE"
                        }
                        l ++ List(
                            Line(s"// Partition $pIndex, phase $cIndex:"),
                            Line(s"// Calling port $port, time bound $execTime (ticks: $numTicks)"),
                            Line(s"phaser_$pIndex.register_phased($outputChannel, $numTicks);")
                        )
                    }
                }
                li ++ regs
            }
        }
        getFnMember(
            comment,
            name,
            List(),
            body,
        )
    }

    private def getFnMember(
        comment: String,
        name: String,
        params: List[CppDoc.Function.Param],
        body: List[Line]
    ): CppDoc.Member.Function = {
        val ll = body match {
        // Force code generation in the no-op case
        // This supports a pattern of manual topology setup
        // See issue fprime-community/fpp#239
        case Nil => lines("// Nothing to do")
        case _ => body
        }
        CppDoc.Member.Function(
            CppDoc.Function(
                Some(comment),
                name,
                params,
                CppDoc.Type("void"),
                ll
            )
        )
    }
}
