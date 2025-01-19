// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)

package tech.rocksavage.chiselware.uart

import chisel3._
import tech.rocksavage.chiselware.uart.param.UartParams

class UartInner(params: UartParams) extends Module {
  val io = IO(new Bundle {})
}