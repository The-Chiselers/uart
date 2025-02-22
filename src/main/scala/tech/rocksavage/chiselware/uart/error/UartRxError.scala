// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)

package tech.rocksavage.chiselware.uart.error

import chisel3._

object UartRxError extends ChiselEnum {
    val None, StartBitError, StopBitError, ParityError = Value
}
