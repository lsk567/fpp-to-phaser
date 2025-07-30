package fpp.compiler.analysis

/**
  * Map from a rate group instance to a list of tasks,
  * each of which is a downstream port being called
  * within a period and the output Sched port channel index.
  * 
  * FIXME: The 2nd element, ie. rate group output channel,
  * appears to be unused.
  */
type Task = (Connection.Endpoint, Int)
type TaskMap = Map[Symbol.ComponentInstance, List[Task]]

case class PhaserAnalysis(
    // Analysis object from CheckSemantics
    analysis: Analysis = Analysis(),
    // Map from a rate group instance name to its period.
    periodMap: Map[Symbol.ComponentInstance, Time] = Map(),
    // Map from a rate group instance name to its offset.
    offsetMap: Map[Symbol.ComponentInstance, Time] = Map(),
    // Map from a port instance name to its deadline / time budget.
    deadlineMap: Map[String, Time] = Map(),
    // Map from a rate group instance to a list of tasks.
    taskMap: TaskMap = Map(),
    // A solved hyperperiod
    hyperperiod: Hyperperiod.Hyperperiod = (List(), 0, ZERO),
    // DAG
    dag: Dag = Dag.empty,
    // Schedule
    schedule: Schedule = List(),
    // List of maps for phasers. Each map maps a task to a phaser output port.
    // This can only be populated after a schedule is obtained.
    phaserPortMaps: List[Map[Task, Int]] = List(),
    // Linux timer (tick) period
    tick: Time = MS(1)
)