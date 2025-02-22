package tech.rocksavage.chiselware.uart.bundle

import chisel3._
import tech.rocksavage.chiselware.uart.error.UartError
import tech.rocksavage.chiselware.uart.param.UartParams

/** Combined I/O bundle for both UART receiver and transmitter.
  *
  * @param params
  *   Configuration parameters for the UART.
  */
class UartBundle(params: UartParams) extends Bundle {
    // UART receiver signals
    // 'rx' is the asynchronous serial input.
    val rx = Input(Bool())
    val read = Input(Bool()) 
    // Data received from RX.
    val dataOut = Output(UInt(params.maxOutputBits.W))
    // Indicates that the received data is valid.
    val valid = Output(Bool())
    // Error signal from the receiver.
    val error = Output(new UartError())

    // Config Signals
    val txControlBundle = new UartTxControlBundle(params)
    val rxControlBundle = new UartRxControlBundle(params)

    val txFifoStatus = Output(new FifoStatusBundle(params))
    val rxFifoStatus = Output(new FifoStatusBundle(params))

    // UART serial output
    val tx = Output(Bool())
}
