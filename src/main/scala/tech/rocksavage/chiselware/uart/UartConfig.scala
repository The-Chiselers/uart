package tech.rocksavage.chiselware.uart

import tech.rocksavage.chiselware.uart.param.UartParams
import tech.rocksavage.traits.ModuleConfig

class UartConfig extends ModuleConfig {
    override def getDefaultConfigs: Map[String, Any] = Map(
      "default" -> Seq(
        UartParams(
          addressWidth = 32,
          dataWidth = 32,
          maxClocksPerBit = 217,
          maxOutputBits = 8,
          syncDepth = 2
        ),
        false
      )
    )
}
