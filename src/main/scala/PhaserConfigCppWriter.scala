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
        List()
    }
}
