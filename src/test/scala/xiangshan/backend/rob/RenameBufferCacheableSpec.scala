package xiangshan.backend.rob

import chisel3._
import chisel3.experimental.cacheable.CacheableModule
import chisel3.stage.ChiselCircuitAnnotation
import chisel3.stage.phases.xiangshan.PrintModuleName
import circt.stage.ChiselStage
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import top.DefaultConfig
import utility._
import xiangshan._

class RenameBufferCacheableTop(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {})

  private val rabSize = p(XSCoreParamsKey).RabSize
  private val rabs = Seq.tabulate(2) { _ =>
    CacheableModule(new RenameBuffer(rabSize), rabSize)
  }

  rabs.foreach { rab =>
    rab.io.redirect.valid := false.B
    rab.io.req.foreach { req =>
      req.valid := false.B
      req.bits := DontCare
    }
    rab.io.fromRob := 0.U.asTypeOf(rab.io.fromRob)
    rab.io.snpt := 0.U.asTypeOf(rab.io.snpt)
  }

  XSLog.collect(0.U, false.B, false.B, false.B)
}

class RenameBufferCacheableSpec extends AnyFlatSpec with Matchers {
  private val base = new DefaultConfig
  private implicit val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head.copy(RobSize = 16, RabSize = 16)
    case DebugOptionsKey => base(DebugOptionsKey).copy(
      FPGAPlatform = false,
      EnableDifftest = false,
      AlwaysBasicDiff = false,
      FullBasicDiff = false,
      EnableDebug = false,
      EnablePerfDebug = true
    )
    case LogUtilsOptionsKey => LogUtilsOptions(
      enableDebug = false,
      enablePerf = true,
      fpgaPlatform = false,
      enableXMR = false
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = true,
      enablePerfDB = false,
      perfLevel = XSPerfLevel.VERBOSE,
      perfDBHartID = 0
    )
  }

  private def declarationCount(chirrtl: String, kind: String, name: String): Int = {
    val declaration = raw"(?m)^\s*$kind ${name}(?:_\d+)?\s*:".r
    declaration.findAllMatchIn(chirrtl).size
  }

  private def sourceConnectionCount(chirrtl: String, source: String): Int = {
    val connection = raw"(?m)^\s*connect logEndpoint\.x_data(?:_\d+)?(?:\.\w+)?, $source\.x_\w+(?:\.\w+)?\b".r
    connection.findAllMatchIn(chirrtl).size
  }

  behavior of "cacheable RenameBuffer diagnostics"

  it should "route every cached instance to distinct endpoint logs and counters" in {
    val elaborated = ChiselStage.elaborate(new RenameBufferCacheableTop)
    val transformed = new PrintModuleName().transform(Seq(ChiselCircuitAnnotation(elaborated)))
      .collectFirst { case annotation: ChiselCircuitAnnotation => annotation.elaboratedCircuit }
      .get
    val chirrtl = transformed.serialize

    raw"(?m)^\s*module RenameBuffer\s*:".r.findAllMatchIn(chirrtl).size shouldBe 1
    raw"(?m)^\s*inst rabs_\d+ of RenameBuffer\b".r.findAllMatchIn(chirrtl).size shouldBe 2
    declarationCount(chirrtl, "regreset", "utilizationCounter") shouldBe 2
    declarationCount(chirrtl, "regreset", "s_idle_to_idleCounter") shouldBe 2
    declarationCount(chirrtl, "regreset", "disallow_enq_not_idle_cycleCounter") shouldBe 2
    raw"(?m)^\s*input x_sink(?:_\d+)?\s*:".r.findAllMatchIn(chirrtl).size shouldBe 6
    sourceConnectionCount(chirrtl, "rabs_0") shouldBe 18
    sourceConnectionCount(chirrtl, "rabs_1") shouldBe 18
    "wrong one-hot reg between".r.findAllMatchIn(chirrtl).size shouldBe 2
    "snapshots should not be empty".r.findAllMatchIn(chirrtl).size shouldBe 2
    "deqPtr is older than enqPtr".r.findAllMatchIn(chirrtl).size shouldBe 2
    raw"\[ERROR\]\[time=%d\] RenameBufferCacheableTop\.rabs_\d(?:\.walkPtrSnapshots_snapshotGen)?:".r
      .findAllMatchIn(chirrtl)
      .size shouldBe 6
  }
}
