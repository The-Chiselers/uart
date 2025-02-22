// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)
package tech.rocksavage.chiselware.uart.bundle

import chisel3._
import tech.rocksavage.chiselware.uart.param.UartParams
import tech.rocksavage.chiselware.uart.error.UartTxError


/** Bundle defining the I/O for a UART transmitter.
  *
  * @param params
  *   Configuration parameters for the UART.
  */
class UartTxBundle(params: UartParams) extends Bundle {
    val txConfig = new UartTxControlBundle(params)

    // The UART serial output.
    val tx = Output(Bool())

    val error = Output(UartTxError())
}
