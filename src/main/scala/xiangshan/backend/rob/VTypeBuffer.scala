package xiangshan.backend.rob

import chisel3._
import chisel3.experimental.cacheable.{CacheableKey, CacheableModule}
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan.backend.Bundles.EnqRobUop
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.rename.SnapshotGenerator
import xiangshan._

class VTypeBufferPtr(size: Int) extends CircularQueuePtr[VTypeBufferPtr](size) {
  def this()(implicit p: Parameters) = this(p(XSCoreParamsKey).VTypeBufferSize)
}

object VTypeBufferPtr {
  def apply(flag: Boolean = false, v: Int = 0)(implicit p: Parameters): VTypeBufferPtr = {
    val ptr = Wire(new VTypeBufferPtr(p(XSCoreParamsKey).VTypeBufferSize))
    ptr.flag := flag.B
    ptr.value := v.U
    ptr
  }
}

class VTypeBufferEntry(implicit p: Parameters) extends XSBundle {
  val vtype = new VType()
  val isVsetvl = Bool()
  val vlWen = Bool()
  val pdestVl = UInt(VlPhyRegIdxWidth.W)
}

class VTypeBufferIO(size: Int)(implicit p: Parameters) extends XSBundle {
  val redirect = Input(ValidIO(new Bundle{}))

  val req = Vec(RenameWidth, Flipped(ValidIO(new EnqRobUop)))

  val fromRob = new Bundle {
    val walkSize = Input(UInt(log2Up(size).W))
    val walkEnd = Input(Bool())
    val commitSize = Input(UInt(log2Up(size).W))
  }

  val snpt = Input(new SnapshotPort)

  val canEnq = Output(Bool())
  val canEnqForDispatch = Output(Bool())

  val commits = Output(new VlCommitBundle(RabCommitWidth))
  val diffCommits = Option.when(backendParams.basicDebugEn)(Output(new DiffVlCommitBundle(CommitWidth)))

  val toDecode = Output(new Bundle {
    val isResumeVType = Bool()
    val walkToArchVType = Bool()
    val walkVType = ValidIO(VType())
    val commitVType = new Bundle {
      val vtype = ValidIO(VType())
      val hasVsetvl = Bool()
    }
  })

  val status = Output(new Bundle {
    val walkEnd = Bool()
  })
}

object VTypeBuffer {
  implicit object Key extends CacheableKey[VTypeBuffer] {
    override def cacheKey(args: Seq[Any]): Any = args.headOption.getOrElse(0)
  }
}

class VTypeBuffer(size: Int)(implicit p: Parameters)
    extends XSModule with HasCircularQueuePtrHelper with CacheableModule {
  val io = IO(new VTypeBufferIO(size))

  protected def buildModule(): Unit = {

  // alias
  val useSnpt = io.snpt.useSnpt
  val snptSelect = io.snpt.snptSelect

  val s_idle :: s_spcl_walk :: s_walk :: Nil = Enum(3)
  val state = RegInit(s_idle)
  val stateNext = WireInit(state) // otherwise keep state value
  val stateLast = RegEnable(state, state =/= stateNext)
  val stateLastCycle = RegNext(state)

  // +1 read port to get walk initial state
  val vtypeBuffer = Reg(Vec(size, new VTypeBufferEntry()))
//  val vtypeBuffer = Module(new SyncDataModuleTemplate(new VTypeBufferEntry(), size, numWrite = RenameWidth, numRead = CommitWidth))

  val vtypeBufferReadAddrVec = Wire(Vec(RabCommitWidth, UInt(log2Ceil(size).W)))
  val vtypeBufferReadDataVec = Wire(Vec(RabCommitWidth, new VTypeBufferEntry()))
  val vtypeBufferWriteEnVec = Wire(Vec(RenameWidth, Bool()))
  val vtypeBufferWriteAddrVec = Wire(Vec(RenameWidth, UInt(log2Ceil(size).W)))
  val vtypeBufferWriteDataVec = Wire(Vec(RenameWidth, new VTypeBufferEntry()))

  val vtypeBufferWenVec: Vec[Bool] = VecInit(vtypeBuffer.indices.map {
    case i =>
      Mux1H(vtypeBufferWriteEnVec zip vtypeBufferWriteAddrVec map {
        case (wen, waddr) =>
          wen -> (waddr === i.U)
      })
  })

  val commitValidVec = Wire(Vec(RabCommitWidth, Bool()))
  val walkValidVec = Wire(Vec(RabCommitWidth, Bool()))
  val infoVec = Wire(Vec(RabCommitWidth, VType()))
  val hasVsetvlVec = Wire(Vec(RabCommitWidth, Bool()))
  val pdestVlVec = Wire(Vec(RabCommitWidth, UInt(VlPhyRegIdxWidth.W)))

  val vtypeBufferWdataVec: Vec[VTypeBufferEntry] = VecInit(vtypeBuffer.indices.map {
    case i =>
      Mux1H(vtypeBufferWriteEnVec zip vtypeBufferWriteAddrVec zip vtypeBufferWriteDataVec map {
        case ((wen, waddr), wdata) =>
          (wen && (waddr === i.U)) -> wdata
      })
  })

  for (i <- vtypeBuffer.indices) {
    when (vtypeBufferWenVec(i)) {
      vtypeBuffer(i) := vtypeBufferWdataVec(i)
    }
  }

  for (i <- vtypeBufferReadDataVec.indices) {
    vtypeBufferReadDataVec(i) := vtypeBuffer(vtypeBufferReadAddrVec(i))
  }

  // pointer
  val enqPtrVec = RegInit(VecInit.tabulate(RenameWidth)(idx => VTypeBufferPtr(flag = false, idx)))
  val enqPtr = enqPtrVec.head
  val enqPtrOH = RegInit(1.U(size.W))
  val enqPtrOHShift = CircularShift(enqPtrOH)
  // may shift [0, RenameWidth] steps
  val enqPtrOHVec = VecInit.tabulate(RenameWidth + 1)(enqPtrOHShift.left)
  val enqPtrVecNext = WireInit(enqPtrVec)

  val deqPtrVec = RegInit(VecInit.tabulate(RabCommitWidth)(idx => VTypeBufferPtr(flag = false, idx)))
  val deqPtr = deqPtrVec.head
  val deqPtrOH = RegInit(1.U(size.W))
  val deqPtrOHShift = CircularShift(deqPtrOH)
  val deqPtrOHVec = VecInit.tabulate(RabCommitWidth + 1)(deqPtrOHShift.left)
  val deqPtrVecNext = WireInit(deqPtrVec)
  XSError(deqPtr.toOH =/= deqPtrOH, p"wrong one-hot reg between $deqPtr and $deqPtrOH")

  val walkPtrVec = RegInit(VecInit.tabulate(RabCommitWidth)(idx => VTypeBufferPtr(flag = false, idx)))
  val walkPtr = Reg(new VTypeBufferPtr)
  val walkPtrOH = walkPtr.toOH
  val walkPtrOHVec = VecInit.tabulate(RabCommitWidth + 1)(CircularShift(walkPtrOH).left)
  val walkPtrNext = Wire(new VTypeBufferPtr)
  val walkPtrVecNext = VecInit((0 until RabCommitWidth).map(x => walkPtrNext + x.U))

  val diffPtr = RegInit(VTypeBufferPtr())
  val diffPtrNext = Wire(chiselTypeOf(diffPtr))

  // get enque vtypes in io.req
  val enqVTypes = VecInit(io.req.map(req => req.bits.vpu.specVType))
  val enqValids = VecInit(io.req.map(_.valid))
  val enqVType = PriorityMux(enqValids.zip(enqVTypes).map { case (valid, vtype) => valid -> vtype })

  val walkPtrSnapshots = SnapshotGenerator(enqPtr, io.snpt.snptEnq, io.snpt.snptDeq, io.redirect.valid, io.snpt.flushVec)
  val walkVTypeSnapshots = SnapshotGenerator(enqVType, io.snpt.snptEnq, io.snpt.snptDeq, io.redirect.valid, io.snpt.flushVec)

  val robWalkEndReg = RegInit(false.B)
  val robWalkEnd = io.fromRob.walkEnd || robWalkEndReg

  when(io.redirect.valid) {
    robWalkEndReg := false.B
  }.elsewhen(io.fromRob.walkEnd) {
    robWalkEndReg := true.B
  }

  // There are two uops mapped to one vset inst.
  // Only record the last here.
  val needAllocVec = VecInit(io.req.map(req => req.valid && req.bits.vlWen))
  val enqCount = PopCount(needAllocVec)

  val commitCount   = Wire(UInt(RabCommitWidth.U.getWidth.W))
  val walkCount     = Wire(UInt(RabCommitWidth.U.getWidth.W))
  val spclWalkCount = Wire(UInt(RabCommitWidth.U.getWidth.W))

  val commitSize   = RegInit(0.U(size.U.getWidth.W))
  val walkSize     = RegInit(0.U(size.U.getWidth.W))
  val spclWalkSize = RegInit(0.U(size.U.getWidth.W))

  val commitSizeNext   = Wire(UInt(size.U.getWidth.W))
  val walkSizeNext     = Wire(UInt(size.U.getWidth.W))
  val spclWalkSizeNext = Wire(UInt(size.U.getWidth.W))

  val newCommitSize   = io.fromRob.commitSize
  val newWalkSize     = io.fromRob.walkSize
  val newSpclWalkSize = Mux(io.redirect.valid && !io.snpt.useSnpt, commitSizeNext, 0.U)

  commitSizeNext   := commitSize + newCommitSize - commitCount
  walkSizeNext     := walkSize + newWalkSize - walkCount
  spclWalkSizeNext := spclWalkSize + newSpclWalkSize - spclWalkCount

  commitSize := Mux(io.redirect.valid && !io.snpt.useSnpt, 0.U, commitSizeNext)
  spclWalkSize := spclWalkSizeNext
  walkSize := Mux(io.redirect.valid, 0.U, walkSizeNext)

  walkPtrNext := MuxCase(walkPtr, Seq(
    (state === s_idle && stateNext === s_walk) -> walkPtrSnapshots(snptSelect),
    (state === s_spcl_walk && stateNext === s_walk) -> deqPtrVecNext.head,
    (state === s_walk && io.snpt.useSnpt && io.redirect.valid) -> walkPtrSnapshots(snptSelect),
    (state === s_walk) -> (walkPtr + walkCount),
  ))

  walkPtr := walkPtrNext

  diffPtr := diffPtrNext
  diffPtrNext := diffPtr + newCommitSize

  val useSnapshotNext = WireInit(false.B)

  useSnapshotNext := (state === s_idle && stateNext === s_walk) || (state === s_walk && io.snpt.useSnpt && io.redirect.valid)
  val useSnapshot = RegNext(useSnapshotNext)
  val snapshotVType = RegEnable(walkVTypeSnapshots(snptSelect), useSnapshotNext)

  // update enq ptr
  val enqPtrNext = Mux(
    state === s_walk && stateNext === s_idle,
    walkPtrNext,
    enqPtr + enqCount
  )

  val enqPtrOHNext = Mux(
    state === s_walk && stateNext === s_idle,
    walkPtrNext.toOH,
    enqPtrOHVec(enqCount)
  )

  enqPtrOH := enqPtrOHNext
  enqPtrVecNext.zipWithIndex.map{ case(ptr, i) => ptr := enqPtrNext + i.U }
  enqPtrVec := enqPtrVecNext

  // update deq ptr
  val deqPtrSteps = Mux1H(Seq(
    (state === s_idle) -> commitCount,
    (state === s_spcl_walk) -> spclWalkCount,
  ))

  val deqPtrNext = deqPtr + deqPtrSteps
  val deqPtrOHNext = deqPtrOHVec(deqPtrSteps)
  deqPtrOH := deqPtrOHNext
  deqPtrVecNext.zipWithIndex.map{ case(ptr, i) => ptr := deqPtrNext + i.U }
  deqPtrVec := deqPtrVecNext

  val allocPtrVec: Vec[VTypeBufferPtr] = VecInit((0 until RenameWidth).map(i => enqPtrVec(PopCount(needAllocVec.take(i)))))
  val vtypeBufferReadPtrVecNext: Vec[VTypeBufferPtr] = Mux1H(Seq(
    (stateNext === s_idle) -> deqPtrVecNext,
    (stateNext === s_walk) -> walkPtrVecNext,
    (stateNext === s_spcl_walk) -> deqPtrVecNext,
  ))

  /**
   * connection of [[vtypeBuffer]]
   */
  vtypeBufferWriteAddrVec := allocPtrVec.map(_.value)
  vtypeBufferWriteEnVec := needAllocVec
  vtypeBufferWriteDataVec.zip(io.req.map(_.bits)).foreach { case (entry: VTypeBufferEntry, inst) =>
    entry.vtype := inst.vpu.vtype
    entry.isVsetvl := VSETOpType.isVsetvl(inst.fuOpType)
    entry.vlWen := inst.vlWen
    entry.pdestVl := inst.pdestVl
  }

  for (i <- vtypeBufferReadAddrVec.indices) {
    vtypeBufferReadAddrVec(i) := RegEnable(
      vtypeBufferReadPtrVecNext(i).value,
      commitValidVec(i) || walkValidVec(i) || io.fromRob.commitSize =/= 0.U || io.fromRob.walkSize =/= 0.U
    )
  }


  for (i <- 0 until RabCommitWidth) {
    commitValidVec(i) := state === s_idle && i.U < commitSize || state === s_spcl_walk && i.U < spclWalkSize
    walkValidVec(i) := state === s_walk && i.U < walkSize || state === s_spcl_walk && i.U < spclWalkSize

    infoVec(i) := vtypeBufferReadDataVec(i).vtype
    hasVsetvlVec(i) := vtypeBufferReadDataVec(i).isVsetvl
    pdestVlVec(i) := vtypeBufferReadDataVec(i).pdestVl
  }

  commitCount   := Mux(state === s_idle,      PopCount(commitValidVec), 0.U)
  walkCount     := Mux(state === s_walk,      PopCount(walkValidVec), 0.U)
  spclWalkCount := Mux(state === s_spcl_walk, PopCount(walkValidVec), 0.U)

  val walkEndNext = walkSizeNext === 0.U
  val spclWalkEndNext = spclWalkSizeNext === 0.U

  state := stateNext

  when (io.redirect.valid) {
    when (io.snpt.useSnpt) {
      stateNext := s_walk
    }.otherwise {
      stateNext := s_spcl_walk
    }
  }.otherwise {
    switch (state) {
      is(s_idle) {
        stateNext := s_idle
      }
      is(s_spcl_walk) {
        when (spclWalkEndNext) {
          stateNext := s_walk
        }
      }
      is(s_walk) {
        when (robWalkEnd && walkEndNext) {
          stateNext := s_idle
        }
      }
    }
  }

  val numValidEntries = distanceBetween(enqPtr, deqPtr)
  val allowEnqueue = GatedValidRegNext(
    numValidEntries + enqCount <= (size - RenameWidth).U,
    true.B
  )
  val allowEnqueueForDispatch = GatedValidRegNext(
    numValidEntries + enqCount <= (size - 2*RenameWidth).U,
    true.B
  )

  val decodeResumeVType = RegInit(0.U.asTypeOf(new ValidIO(VType())))
  val newestVType = PriorityMux(walkValidVec.zip(infoVec).map { case(walkValid, info) => walkValid -> info }.reverse)
  val newestArchVType = PriorityMux(commitValidVec.zip(infoVec).map { case(commitValid, info) => commitValid -> info }.reverse)
  val commitVTypeValid = commitValidVec.asUInt.orR
  val walkToArchVType = RegInit(false.B)

  walkToArchVType := false.B

  when (state === s_spcl_walk) {
    // special walk use commit vtype
    decodeResumeVType.valid := commitVTypeValid
    decodeResumeVType.bits := newestArchVType
  }.elsewhen (useSnapshot) {
    // use snapshot vtype
    decodeResumeVType.valid := true.B
    decodeResumeVType.bits := snapshotVType
  }.elsewhen (state === s_walk && walkCount =/= 0.U) {
    decodeResumeVType.valid := true.B
    decodeResumeVType.bits := newestVType
  }.elsewhen (state === s_walk && stateLastCycle =/= s_walk) {
    // walk start with arch vtype
    decodeResumeVType.valid := false.B
    walkToArchVType := true.B
  }.otherwise {
    decodeResumeVType.valid := false.B
  }

  io.canEnq := allowEnqueue && state === s_idle
  io.canEnqForDispatch := allowEnqueueForDispatch && state === s_idle

  io.commits.isCommit := state === s_idle || state === s_spcl_walk
  io.commits.isWalk := state === s_walk || state === s_spcl_walk
  for (i <- 0 until RabCommitWidth) {
    io.commits.commitValid(i) := Mux1H(Seq(
      (state === s_idle) -> (i.U < commitSize),
      (state === s_spcl_walk) -> (i.U < spclWalkSize),
    ))
    io.commits.walkValid(i) := Mux1H(Seq(
      (state === s_walk) -> (i.U < walkSize),
      (state === s_spcl_walk) -> (i.U < spclWalkSize),
    ))
    io.commits.pdestVl(i) := pdestVlVec(i)
  }

  io.diffCommits.foreach {
    diffCommits =>
      for (i <- diffCommits.commitValid.indices) {
        diffCommits.commitValid(i) := i.U < newCommitSize
        diffCommits.pdestVl(i) := vtypeBuffer((diffPtr + i.U).value).pdestVl
      }
  }

  io.status.walkEnd := walkEndNext
  // update vtype in decode when VTypeBuffer resumes from walk state
  // note that VTypeBuffer can still send resuming request in the first cycle of s_idle
  io.toDecode.isResumeVType := state =/= s_idle || decodeResumeVType.valid
  io.toDecode.walkVType.valid := decodeResumeVType.valid
  io.toDecode.walkVType.bits := Mux(io.toDecode.walkVType.valid, decodeResumeVType.bits, 0.U.asTypeOf(VType()))

  io.toDecode.commitVType.vtype.valid := commitVTypeValid
  io.toDecode.commitVType.vtype.bits := newestArchVType

  io.toDecode.walkToArchVType := walkToArchVType

  // because vsetvl flush pipe, there is only one vset instruction when vsetvl is committed
  val hasVsetvl = commitValidVec.zip(hasVsetvlVec).map { case(commitValid, hasVsetvl) => commitValid && hasVsetvl }.reduce(_ || _)
  io.toDecode.commitVType.hasVsetvl := hasVsetvl

  XSError(isBefore(enqPtr, deqPtr) && !isFull(enqPtr, deqPtr), "\ndeqPtr is older than enqPtr!\n")

  QueuePerf(size, numValidEntries, numValidEntries === size.U)

  XSPerfAccumulate("s_idle_to_idle", state === s_idle      && stateNext === s_idle)
  XSPerfAccumulate("s_idle_to_swlk", state === s_idle      && stateNext === s_spcl_walk)
  XSPerfAccumulate("s_idle_to_walk", state === s_idle      && stateNext === s_walk)
  XSPerfAccumulate("s_swlk_to_idle", state === s_spcl_walk && stateNext === s_idle)
  XSPerfAccumulate("s_swlk_to_swlk", state === s_spcl_walk && stateNext === s_spcl_walk)
  XSPerfAccumulate("s_swlk_to_walk", state === s_spcl_walk && stateNext === s_walk)
  XSPerfAccumulate("s_walk_to_idle", state === s_walk      && stateNext === s_idle)
  XSPerfAccumulate("s_walk_to_swlk", state === s_walk      && stateNext === s_spcl_walk)
  XSPerfAccumulate("s_walk_to_walk", state === s_walk      && stateNext === s_walk)

  dontTouch(enqPtrVec)
  dontTouch(deqPtrVec)
  dontTouch(deqPtr)
  dontTouch(numValidEntries)
  dontTouch(commitCount)
  dontTouch(walkCount)
  dontTouch(spclWalkCount)
  dontTouch(commitSize)
  dontTouch(walkSize)
  dontTouch(spclWalkSize)
  dontTouch(commitSizeNext)
  dontTouch(walkSizeNext)
  dontTouch(spclWalkSizeNext)
  dontTouch(newCommitSize)
  dontTouch(newWalkSize)
  dontTouch(newSpclWalkSize)
  dontTouch(commitValidVec)
  dontTouch(walkValidVec)
  dontTouch(infoVec)
  }
}






