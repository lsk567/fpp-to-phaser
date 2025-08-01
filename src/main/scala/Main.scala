package fpp.compiler.tools

import fpp.compiler.analysis._
import fpp.compiler.ast._
import fpp.compiler.codegen._
import fpp.compiler.syntax._
import fpp.compiler.transform._
import fpp.compiler.util._
import scopt.OParser

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

object FPPToPhaser {

  case class Options(
    numPhasers: Int = 1,
    dir: Option[String] = None,
    files: List[File] = Nil,
    imports: List[File] = Nil,
  )

  def command(options: Options) = {
    val files = options.files.reverse match {
      // case Nil => List(File.StdIn) // FIXME: Causing a hang for some reason
      case Nil => List()
      case list => list
    }
    val a = Analysis(inputFileSet = options.files.toSet)
    for {
      _ <- {
        println("Number of phasers requested: " + options.numPhasers)
        Right(())
      }
      // Get a list of FPP files from the command line
      // and resolve include specifiers.
      tulFiles <- Result.map(files, Parser.parseFile (Parser.transUnit) (None) _)
      _ <- {
        println("Resolving includes")
        Right(())
      }
      aTulFiles <- ResolveSpecInclude.transformList(
        a,
        tulFiles, 
        ResolveSpecInclude.transUnit
      )
      tulFiles <- Right(aTulFiles._2)
      _ <- {
        println("Resolving imports")
        Right(())
      }
      // Get a list of auxiliary FPP files that complete
      // the FPP model, so that analysis can be done.
      tulImports <- Result.map(
        options.imports,
        Parser.parseFile (Parser.transUnit) (None) _
      )
      _ <- {
        println("Checking semantics")
        Right(())
      }
      // Perform analysis on the complete model.
      // Receive an Analysis object.
      a <- CheckSemantics.tuList(a, tulFiles ++ tulImports)

      // Extract all rate group info from instance.fpp and annotations.
      pa <- {
        val pa = PhaserAnalysis(analysis = a)
        val result = AnnotationCollector.tuList(pa, tulFiles)
        // Print state maps to sanity check.
        result.map { pa =>
          println("Period map:")
          pa.periodMap.foreach { case (key, value) => println(s"$key -> $value") }
          println("Offset map:")
          pa.offsetMap.foreach { case (key, value) => println(s"$key -> $value") }
          println("Deadline map:")
          pa.deadlineMap.foreach { case (key, value) => println(s"$key -> $value") }
        }
        result
      }

      // Collect all rate group connections.
      pa <- ConnectionCollector.tuList(pa, tulFiles)

      // Compute SSFA by unrolling rate group execution.
      pa <- {
        val initStep = Hyperperiod.initStep(pa)
        val hp = Hyperperiod.solve
          (pa, initStep, List())
          (Hyperperiod.next, Hyperperiod.hpCheck)
        println("Writing hyperperiod.txt to " + System.getProperty("user.dir"))
        Files.write(Paths.get("hyperperiod.txt"), hp.toString.getBytes(StandardCharsets.UTF_8))
        val dot = Hyperperiod.toDot(hp)
        println("Writing hyperperiod.dot to " + System.getProperty("user.dir"))
        Files.write(Paths.get("hyperperiod.dot"), dot.getBytes(StandardCharsets.UTF_8))
        Right(pa.copy(hyperperiod = hp))
      }

      // Generate a DAG from the hyperperiod.
      // FIXME: Assume there is one DAG for now.
      pa <- {
        val dag = Dag.fromHyperperiod(pa.taskMap, pa.hyperperiod)
        println("Print edges")
        println(dag.edges)
        val dot = dag.toDot
        println("Writing dag.dot to " + System.getProperty("user.dir"))
        Files.write(Paths.get("dag.dot"), dot.getBytes(StandardCharsets.UTF_8))
        Right(pa.copy(dag = dag))
      }

      // Partition the schedule using a list scheduler,
      // and validate the schedule during the process.
      pa <- {
        val sched = Scheduler.schedule(pa.dag, n=pa.n, pa, mapEntireRateGroup=true).getOrElse(List.empty)
        val phaserPortMaps = Scheduler.assignPhaserPorts(sched)
        println(s"phaserPortMaps: $phaserPortMaps")
        val dot = pa.dag.toDotScheduled(sched)
        println("Writing dagS.dot to " + System.getProperty("user.dir"))
        Files.write(Paths.get("dagS.dot"), dot.getBytes(StandardCharsets.UTF_8))
        Right(pa.copy(schedule = sched, phaserPortMaps = phaserPortMaps))
      }

      // Generate phaser configurations.
      _ <- {
        val dir = options.dir match {
          case Some(dir1) => dir1
          case None => "."
        }
        val state = CppWriterState(
          a,
          dir,
          toolName = Some(name)
        )
        // Generate phaser config.
        val cppDoc = PhaserConfigCppWriter(state, pa).write
        CppWriter.writeCppDoc(state, cppDoc)
        // Generate phaser FPP files.
        PhaserFppWriter(state, pa).writeInstance
        PhaserFppWriter(state, pa).writeConnections
        Right(())
      }

    } yield ()
  }

  def main(args: Array[String]) =
    Tool(name).mainMethod(args, oparser, Options(), command)

  val builder = OParser.builder[Options]

  val name = "fpp-to-phaser"

  val oparser = {
    import builder._
    OParser.sequence(
      programName(name),
      head(name, Version.v),
      help('h', "help").text("print this message and exit"),
      opt[String]('d', "directory")
        .valueName("<dir>")
        .action((d, c) => c.copy(dir = Some(d)))
        .text("output directory"),
      opt[Seq[String]]('i', "imports")
        .valueName("<file1>,<file2>...")
        .action((i, c) => c.copy(imports = i.toList.map(File.fromString(_))))
        .text("files to import"),
      opt[Int]('n', "num-phasers")
        .valueName("<size>")
        .validate(s => if (s > 0) success else failure("Number of phasers must be greater than zero"))
        .action((s, c) => c.copy(numPhasers = s))
        .text("set the number of phasers to map tasks to"),
      arg[String]("file ...")
        .unbounded()
        .optional()
        .action((f, c) => c.copy(files = File.fromString(f) :: c.files))
        .text("files to translate"),
    )
  }

}