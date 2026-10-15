/***************************************************************************************
* Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
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

package chisel3.stage.phases.xiangshan

import chisel3.stage.ChiselCircuitAnnotation
import chisel3.stage.phases.{AddSerializationAnnotations, Elaborate}
import firrtl.AnnotationSeq
import firrtl.options.{Dependency, Phase}

/** Adds the annotations collected during elaboration to the annotation list.
  *
  * `chisel3.stage.phases.Convert` publishes them as a side effect, but it does so while converting
  * the circuit to the deprecated FIRRTL IR, which cannot represent `DefParamInstanceChoice` (the
  * per-hart parameterized instance choice). XiangShan needs the choice, so this phase publishes the
  * annotations without the legacy conversion. Without it the emitted FIRRTL loses `dontTouch`,
  * `BlackBoxInlineAnno` (inline external module Verilog) and decode-table annotations.
  */
@scala.annotation.nowarn("msg=All APIs in package firrtl are deprecated")
class AddCircuitAnnotations extends Phase {

  override def prerequisites = Seq(Dependency[Elaborate])
  // Must run before the annotations are consumed by the serialization and CIRCT phases.
  override def optionalPrerequisiteOf = Seq(
    Dependency[AddSerializationAnnotations],
    Dependency[circt.stage.phases.CIRCT]
  )
  override def invalidates(a: Phase) = false

  def transform(annotations: AnnotationSeq): AnnotationSeq = annotations.flatMap {
    case a: ChiselCircuitAnnotation => Some(a) ++ a.elaboratedCircuit.annotations
    case a                          => Some(a)
  }
}
