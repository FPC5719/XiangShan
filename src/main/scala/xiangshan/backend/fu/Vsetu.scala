/***************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package xiangshan.backend.fu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.cacheable.{CacheableKey, CacheableModule}
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.vector.Bundles.{VConfig, VType, Vl, VSew, VLmul, VsetVType}

class VsetModuleIO(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val vlWidth = p(XSCoreParamsKey).vlWidth

  val in = Input(new Bundle {
    val avl   : UInt = UInt(XLEN.W)
    val vtype : VsetVType = VsetVType()
    val func  : UInt = FuOpType()
    val oldVt = Option.when(cfg.readOldVtype)(VType())
  })

  val out = Output(new Bundle {
    val vconfig: VConfig = VConfig()
    val vlmax  : UInt = UInt(vlWidth.W)
  })

  // test bundle for internal state
  val testOut = Output(new Bundle {
    val log2Vlmax : UInt = UInt(3.W)
    val vlmax     : UInt = UInt(vlWidth.W)
  })
}

object VsetModule {
  implicit object Key extends CacheableKey[VsetModule] {
    override def cacheKey(args: Seq[Any]): Any = args.headOption.getOrElse(false)
  }
}

class VsetModule(cfg: FuConfig)(implicit p: Parameters) extends XSModule with CacheableModule {
  val io = IO(new VsetModuleIO(cfg))

  protected def buildModule(): Unit = {

  val avl   = io.in.avl
  val func  = io.in.func
  val vtype = io.in.vtype

  val outVConfig = io.out.vconfig

  val vlWidth = p(XSCoreParamsKey).vlWidth

  val isSetVlmax = VSETOpType.isSetVlmax(func)
  val isVsetivli = VSETOpType.isVsetivli(func)
  val isKeepVl   = VSETOpType.isKeepVl(func)

  val vlmul: UInt = vtype.vlmul
  val vsew : UInt = vtype.vsew

  val vl = WireInit(0.U(XLEN.W))

  // EncodedLMUL = log(LMUL)
  // EncodedSEW  = log(SEW) - 3
  //        VLMAX  = VLEN * LMUL / SEW
  // => log(VLMAX) = log(VLEN * LMUL / SEW)
  // => log(VLMAX) = log(VLEN) + log(LMUL) - log(SEW)
  // =>     VLMAX  = 1 << log(VLMAX)
  //               = 1 << (log(VLEN) + log(LMUL) - log(SEW))

  // vlen =  128
  val log2Vlen = log2Up(VLEN)
  println(s"[VsetModule] log2Vlen: $log2Vlen")
  println(s"[VsetModule] vlWidth: $vlWidth")

  val log2Vlmul = vlmul
  // use 2 bits vsew to store vsew
  val log2Vsew = (vsew.take(3) + 3.U).ensuring(_.getWidth == 3)

  // vlen = 128, lmul = 8, sew = 8, log2Vlen = 7,
  // vlmul = b011, vsew = 0, 7 + 3 - (0 + 3) = 7
  // vlen = 128, lmul = 2, sew = 16
  // vlmul = b001, vsew = 1, 7 + 1 - (1 + 3) = 4
  val log2Vlmax: UInt = (log2Vlen.U(3.W) + log2Vlmul - log2Vsew).ensuring(_.getWidth == 3)
  val vlmax = (1.U(vlWidth.W) << log2Vlmax).asUInt

  val normalVL = Mux(avl > vlmax, vlmax, avl)

  vl := Mux(isVsetivli, normalVL, Mux(isSetVlmax, vlmax, normalVL))

  val log2Elen = log2Up(ELEN)
  val log2VsewMax = Mux(log2Vlmul(2), log2Elen.U + log2Vlmul, log2Elen.U)

  val sewIllegal = VSew.isReserved(vsew) || (log2Vsew > log2VsewMax)
  val lmulIllegal = VLmul.isReserved(vlmul)
  val vtypeIllegal = vtype.reserved.orR

  val oldVt = io.in.oldVt.getOrElse(VType.initVtype())

  /* vlmul is not sign extended because overlap impossible: 
   *     | mf8 | mf4 | mf2 | m1  | m2  | m4  | m8  |
   * e8  | 101 | 110 | 111 | 000 | 001 | 010 | 011 |
   * e16 |  R  | 101 | 110 | 111 | 000 | 001 | 010 |
   * e32 |  R  |  R  | 101 | 110 | 111 | 000 | 001 |
   * e64 |  R  |  R  |  R  | 101 | 110 | 111 | 000 |
   */
  val log2NewRatio = vlmul - vsew
  val log2OldRatio = oldVt.vlmul- oldVt.vsew
  val keepVlIllegal: Bool = io.in.oldVt match {
    case Some(_) => isKeepVl && (oldVt.illegal || log2NewRatio =/= log2OldRatio)
    case None => 0.B
  }

  val illegal = lmulIllegal | sewIllegal | vtypeIllegal | vtype.illegal | keepVlIllegal

  outVConfig.vl := Mux(illegal, 0.U, vl)
  outVConfig.vtype.illegal := illegal
  outVConfig.vtype.vta := Mux(illegal, 0.U, vtype.vta)
  outVConfig.vtype.vma := Mux(illegal, 0.U, vtype.vma)
  outVConfig.vtype.vlmul := Mux(illegal, 0.U, vtype.vlmul)
  outVConfig.vtype.vsew := Mux(illegal, 0.U, vtype.vsew)

  val log2VlenDivVsew = log2Vlen.U(3.W) - log2Vsew
  val vlenDivVsew = 1.U(vlWidth.W) << log2VlenDivVsew
  io.out.vlmax := Mux(vlmax >= vlenDivVsew, vlmax, vlenDivVsew)

  io.testOut.vlmax := vlmax
  io.testOut.log2Vlmax := log2Vlmax
  }
}
