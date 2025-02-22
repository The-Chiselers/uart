package tech.rocksavage.chiselware.uart.bundle

import chisel3._
import tech.rocksavage.chiselware.uart.param.UartParams
import chisel3.util._

class FifoStatusBundle(params: UartParams) extends Bundle {
    val full = Bool()
    val empty = Bool()
    val count = UInt(log2Ceil(params.fifoDepth + 1).W)
    val halfFull = Bool()
}