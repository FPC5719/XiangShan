package xiangshan.backend

import top.{ArgParser, BaseConfig, Generator}
import xiangshan.backend.regfile.IntPregParams
import xiangshan.{XSCoreParameters, XSCoreParamsKey, XSTileKey}

object BackendMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--fpga-platform" :+ "--target" :+ "verilog")

  val defaultConfig = config.alterPartial({
    // Get XSCoreParams and pass it to the "small module"
    case XSCoreParamsKey => config(XSTileKey).head
  })

  val backendParams = defaultConfig(XSCoreParamsKey).backendParams
  Generator.execute(
    firrtlOpts :+ "--full-stacktrace" :+ "--target-dir" :+ "backend",
    new Backend(backendParams)(defaultConfig),
    firtoolOpts
  )
  println("done")
}
