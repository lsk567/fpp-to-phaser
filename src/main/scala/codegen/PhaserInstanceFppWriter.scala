package fpp.compiler.codegen

import fpp.compiler.analysis._
import fpp.compiler.util._

case class PhaserInstanceFppWriter(
    s: CppWriterState,
    pa: PhaserAnalysis,
) extends LineUtils {

    def write: Unit = {
        if (pa.n > 8) throw InternalError(s"More than 8 phasers requested: ${pa.n}. This is currently not supported due to running out of base id (0x0200~0x0900).")
        val fileName = s"phaser_instances.fpp"
        val lines = (0 until pa.n).foldLeft(List.empty[Line]) {
            (li, i) => {
                val instance = List(
                    Line(s"instance phaser_$i: Svc.ActivePhaser base id 0x0${i+2}00 \\"),
                    Line(s"  queue size 1 \\"),
                    Line(s"  stack size Default.STACK_SIZE \\"),
                    Line(s"  priority 120"),
                    Line(s"")
                )
                li ++ instance
            }
        }
        writeLinesToFile(s, fileName, lines)
    }

    private def writeLinesToFile(
        s: CppWriterState,
        fileName: String,
        lines: List[Line]
    ) = {
        val path = java.nio.file.Paths.get(s.dir, fileName)
        val file = File.Path(path)
        for (writer <- file.openWrite()) yield {
            lines.map(Line.write(writer) _)
            writer.close()
        }
    }
}
