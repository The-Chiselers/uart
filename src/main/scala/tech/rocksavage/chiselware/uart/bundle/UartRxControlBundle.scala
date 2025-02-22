// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)

package tech.rocksavage.chiselware.uart.bundle

import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.uart.param.UartParams

// from the perspective of the UART which is receiving data
class UartRxControlBundle(params: UartParams) extends Bundle {

    // configuration inputs
    val clocksPerBitDb  = Input(UInt((log2Ceil(params.maxClocksPerBit) + 1).W))
    val numOutputBitsDb = Input(UInt((log2Ceil(params.maxOutputBits) + 1).W))
    val useParityDb     = Input(Bool())
    val parityOddDb     = Input(Bool())
    val clearErrorDb    = Input(Bool())

}
