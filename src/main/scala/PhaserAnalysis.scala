package fpp.compiler.analysis

case class PhaserAnalysis(
    // Analysis object from CheckSemantics
    analysis: Analysis = Analysis(),
    // Map from a rate group instance name to its period.
    periodMap: Map[Symbol.ComponentInstance, Time] = Map(),
    // Map from a rate group instance name to its offset.
    offsetMap: Map[Symbol.ComponentInstance, Time] = Map(),
    // Map from a port instance name to its deadline / time budget.
    deadlineMap: Map[String, Time] = Map(),
    // Map from a rate group instance to a list of tuples,
    // each of which is a downstream port being called
    // within a period and the output Sched port channel index.
    taskMap: Map[Symbol.ComponentInstance, List[(Connection.Endpoint, Int)]] = Map()
)