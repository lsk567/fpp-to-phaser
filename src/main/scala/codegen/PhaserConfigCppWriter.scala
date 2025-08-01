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
        List(getConfigureTopologyFn)
    }

    private def getConfigureTopologyFn: CppDoc.Member.Function = {
        val comment = s"Register phases for each partition."
        val name = s"configureTopology"
        
        val preamble = List(
            Line(s"// Linux timer period (tick): ${pa.tick}"),
            Line(s"// Rate group driver needs a divisor list"),
            Line(s"rateGroupDriverComp.configure(rateGroupDivisorsSet);")
        )
        val phase_conf = pa.schedule.zipWithIndex.foldLeft(List.empty[Line]) {
            case (li, (partition, pIndex)) => {
                val phaserCycles = Time.ratio(
                    pa.hyperperiod._3,
                    pa.tick
                )
                li ++ List(
                    Line(s"// Use $phaserCycles ticks for a hyperperiod of ${pa.hyperperiod._3}"),
                    Line(s"phaser$pIndex.configure($phaserCycles);")
                )
            }
        }
        val phase_regs = pa.schedule.zipWithIndex.foldLeft(List.empty[Line]) {
            case (li, (partition, pIndex)) => {
                // For each call within a partition,
                // generate a registration.
                val regs: (List[Line], Long) = partition.zipWithIndex.foldLeft((List.empty[Line], 0L)) {
                    case ((l, lastStartCycle), (taskNode, cIndex)) => {
                        val phaserOutputChannel = pa.phaserPortMaps(pIndex)(taskNode.task)
                        val port = taskNode.task._1.port.toString
                        val execTime = pa.deadlineMap.get(port)
                        val numTicks: String = execTime match {
                            case Some(t) => Time.ratio(t, pa.tick).toString
                            case None => "Svc::ActivePhaser::DONT_CARE"
                        }
                        val startCycle = Time.ratio(taskNode.time, pa.tick)
                        // Make sure a new start time is only specified once. After this, use DONT_CARE, otherwise
                        // the phaser complains.
                        val startCycleStr: String = if (startCycle > lastStartCycle) then startCycle.toString else "Svc::ActivePhaser::DONT_CARE"
                        (l ++ List(
                            Line(s"// Partition $pIndex, phase $cIndex:"),
                            Line(s"// Calling port $port released at ${taskNode.time}, time bound $execTime (ticks: $numTicks)"),
                            Line(s"phaser$pIndex.register_phased($phaserOutputChannel, $numTicks, $startCycleStr);")
                        ), startCycle)
                    }
                }
                li ++ regs._1
            }
        }
        val body = preamble ++ phase_conf ++ phase_regs
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
