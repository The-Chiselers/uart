package tech.rocksavage.chiselware.uart

import chisel3._
import tech.rocksavage.chiselware.uart.bundle.UartBundle
import tech.rocksavage.chiselware.uart.param.UartParams

/** A merged UART module that includes both the receiver and transmitter.
  *
  * @param params
  *   Configuration parameters for the UART.
  * @param formal
  *   A boolean to enable formal verification.
  */
class UartInner(params: UartParams, formal: Boolean = true) extends Module {
    // Use the combined bundle for I/O
    val io = IO(new UartBundle(params))

    // Instantiate the receiver module
    val rxModule = Module(new UartRx(params, formal))
    // Instantiate the transmitter module
    val txModule = Module(new UartTx(params, formal))

    // Connect the RX side
    rxModule.io.rx   := io.rx
    rxModule.io.read := io.read 
    io.dataOut       := rxModule.io.data
    io.valid         := rxModule.io.valid
    io.error.rxError := rxModule.io.error
    io.error.txError := txModule.io.error

    // Connect FIFO status signals
    io.rxFifoStatus.full := rxModule.fifoStatus.full
    io.rxFifoStatus.empty := rxModule.fifoStatus.empty
    io.rxFifoStatus.count := rxModule.fifoStatus.count
    io.rxFifoStatus.halfFull := rxModule.fifoStatus.halfFull

    io.txFifoStatus.full := txModule.fifoStatus.full
    io.txFifoStatus.empty := txModule.fifoStatus.empty
    io.txFifoStatus.count := txModule.fifoStatus.count
    io.txFifoStatus.halfFull := txModule.fifoStatus.halfFull

    // Connect control signals to RX module
    rxModule.io.rxConfig.clocksPerBitDb  := io.rxControlBundle.clocksPerBitDb
    rxModule.io.rxConfig.numOutputBitsDb := io.rxControlBundle.numOutputBitsDb
    rxModule.io.rxConfig.useParityDb     := io.rxControlBundle.useParityDb
    rxModule.io.rxConfig.parityOddDb     := io.rxControlBundle.parityOddDb
    rxModule.io.rxConfig.clearErrorDb    := io.rxControlBundle.clearErrorDb

    // Connect control signals to TX module
    txModule.io.txConfig.load            := io.txControlBundle.load
    txModule.io.txConfig.data            := io.txControlBundle.data
    txModule.io.txConfig.clocksPerBitDb  := io.txControlBundle.clocksPerBitDb
    txModule.io.txConfig.numOutputBitsDb := io.txControlBundle.numOutputBitsDb
    txModule.io.txConfig.useParityDb     := io.txControlBundle.useParityDb
    txModule.io.txConfig.parityOddDb     := io.txControlBundle.parityOddDb

    // Connect the TX output
    io.tx := txModule.io.tx

    // Debug signals
//    when(rxModule.io.valid) {
//        printf(p"[UartInner.scala DEBUG] RX valid, data=${rxModule.io.data}\n")
//    }

    when(txModule.io.txConfig.load) {
        printf(
          p"[UartInner.scala DEBUG] TX loading data=${txModule.io.txConfig.data}\n"
        )
    }
}
