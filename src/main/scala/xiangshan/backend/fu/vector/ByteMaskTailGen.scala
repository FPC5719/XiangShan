package xiangshan.backend.fu.vector

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.cacheable.{CacheableKey, CacheableModule}
import chisel3.util._
import xiangshan.backend.fu.vector.Bundles.{VSew, Vl}
import xiangshan.backend.fu.vector.utils.{MaskExtractor, UIntToContLow0s, UIntToContLow1s}
import utility.XSDebug
import yunsuan.vector.SewOH
import yunsuan.util.LookupTree


class ByteMaskTailGenIO(vlen: Int)(implicit p: Parameters) extends Bundle {
  val numBytes = vlen / 8
  val maxVLMUL = 8
  val maxVLMAX = 8 * 16 // TODO: parameterize this
  val elemIdxWidth = log2Up(maxVLMAX + 1)
  println(s"elemIdxWidth: $elemIdxWidth")

  val in = Input(new Bundle {
    val begin = UInt(elemIdxWidth.W)
    val end = UInt(elemIdxWidth.W)
    val vma = Bool()
    val vta = Bool()
    val vsew = VSew()
    val maskUsed = UInt(numBytes.W)
    val vdIdx = UInt(3.W)
  })
  val out = Output(new Bundle {
    val activeEn   = UInt(numBytes.W)
    val agnosticEn = UInt(numBytes.W)
  })
  val debugOnly = Output(new Bundle {
    val startBytes = UInt()
    val vlBytes = UInt()
    val prestartEn = UInt()
    val bodyEn = UInt()
    val tailEn = UInt()
    val maskEn = UInt()
    val maskAgnosticEn = UInt()
    val tailAgnosticEn = UInt()
    val agnosticEn = UInt()
  })
}

object ByteMaskTailGen {
  implicit object Key extends CacheableKey[ByteMaskTailGen] {
    override def cacheKey(args: Seq[Any]): Any = args.headOption.getOrElse(0)
  }
}

class ByteMaskTailGen(vlen: Int)(implicit p: Parameters) extends Module with CacheableModule {
  require(isPow2(vlen))

  val numBytes = vlen / 8
  val byteWidth = log2Up(numBytes) // vlen=128, numBytes=16, byteWidth=log2(16)=4
  val maxVLMUL = 8
  val maxVLMAX = 8 * 16 // TODO: parameterize this
  val elemIdxWidth = log2Up(maxVLMAX + 1)

  println(s"numBytes: ${numBytes}, byteWidth: ${byteWidth}")

  val io = IO(new ByteMaskTailGenIO(vlen))

  protected def buildModule(): Unit = {

  val eewOH = SewOH(io.in.vsew).oneHot

  val startBytes = Mux1H(eewOH, Seq.tabulate(4)(x => io.in.begin(elemIdxWidth - 1 - x, 0) << x)).asUInt
  val vlBytes    = Mux1H(eewOH, Seq.tabulate(4)(x => io.in.end(elemIdxWidth - 1 - x, 0) << x)).asUInt
  val vdIdx      = io.in.vdIdx

  val prestartEn = UIntToContLow1s(startBytes, maxVLMAX)
  val bodyEn = UIntToContLow0s(startBytes, maxVLMAX) & UIntToContLow1s(vlBytes, maxVLMAX)
  val tailEn = UIntToContLow0s(vlBytes, maxVLMAX)
  val prestartEnInVd = LookupTree(vdIdx, (0 until maxVLMUL).map(i => i.U -> prestartEn((i+1)*numBytes - 1, i*numBytes)))
  val bodyEnInVd = LookupTree(vdIdx, (0 until maxVLMUL).map(i => i.U -> bodyEn((i+1)*numBytes - 1, i*numBytes)))
  val tailEnInVd = LookupTree(vdIdx, (0 until maxVLMUL).map(i => i.U -> tailEn((i+1)*numBytes - 1, i*numBytes)))

  val maskEn = MaskExtractor(vlen)(io.in.maskUsed, io.in.vsew)
  val maskOffEn = (~maskEn).asUInt
  val maskAgnosticEn = Mux(io.in.vma, maskOffEn, 0.U) & bodyEnInVd

  val tailAgnosticEn = Mux(io.in.vta, tailEnInVd, 0.U)

  val activeEn = Mux(io.in.begin >= io.in.end, 0.U(numBytes.W), bodyEnInVd & maskEn)
  val agnosticEn = Mux(io.in.begin >= io.in.end, 0.U(numBytes.W), maskAgnosticEn | tailAgnosticEn)

  // TODO: delete me later
  dontTouch(eewOH)
  dontTouch(startBytes)
  dontTouch(vlBytes)
  dontTouch(vdIdx)
  dontTouch(prestartEn)
  dontTouch(bodyEn)
  dontTouch(tailEn)
  dontTouch(prestartEnInVd)
  dontTouch(bodyEnInVd)
  dontTouch(tailEnInVd)
  dontTouch(maskEn)
  dontTouch(maskOffEn)
  dontTouch(maskAgnosticEn)
  dontTouch(tailAgnosticEn)

  io.out.activeEn := activeEn
  io.out.agnosticEn := agnosticEn

  io.debugOnly.startBytes := startBytes
  io.debugOnly.vlBytes := vlBytes
  io.debugOnly.prestartEn := prestartEnInVd
  io.debugOnly.bodyEn := bodyEn
  io.debugOnly.tailEn := tailEnInVd
  io.debugOnly.maskEn := maskEn
  io.debugOnly.maskAgnosticEn := maskAgnosticEn
  io.debugOnly.tailAgnosticEn := tailAgnosticEn
  io.debugOnly.agnosticEn := agnosticEn
  }
}
