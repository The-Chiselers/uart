// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)

package tech.rocksavage.chiselware.uart.bundle

import chisel3._
import tech.rocksavage.chiselware.uart.error.UartRxError
import tech.rocksavage.chiselware.uart.param.UartParams

// from the perspective of the UART which is receiving data
class UartRxBundle(params: UartParams) extends Bundle {
    val rx    = Input(Bool())
    val data  = Output(UInt(params.maxOutputBits.W))
    val valid = Output(Bool())
    val ready = Input(Bool())

    val error = Output(UartRxError())

    // configuration inputs

    // clock = 25 MHz
    // baud = 115200
    // 25000000 / 115200 = 217 Clocks Per Bit.
    val rxConfig = new UartRxControlBundle(params)

}
