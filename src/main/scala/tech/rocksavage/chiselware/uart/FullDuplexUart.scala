package tech.rocksavage.chiselware.uart

import chisel3._
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.uart.param.UartParams

class FullDuplexUart(p: UartParams) extends Module {
    val io = IO(new Bundle {
        // Expose separate APB interfaces for each UART
        val uart1Apb = new ApbBundle(ApbParams(p.dataWidth, p.addressWidth))
        val uart2Apb = new ApbBundle(ApbParams(p.dataWidth, p.addressWidth))

        // External UART pins for testing/verification
        val uart1_tx = Output(Bool())
        val uart1_rx = Input(Bool())
        val uart2_tx = Output(Bool())
        val uart2_rx = Input(Bool())
    })

    // Instantiate two complete UART modules
    val uart1 = Module(new Uart(p, false))
    val uart2 = Module(new Uart(p, false))

    // Connect APB interfaces
    uart1.io.apb <> io.uart1Apb
    uart2.io.apb <> io.uart2Apb

    // Direct connection between UARTs
    // UART1's TX goes to UART2's RX
    uart2.io.rx := uart1.io.tx
    // UART2's TX goes to UART1's RX
    uart1.io.rx := uart2.io.tx

    // External connections for testing/debugging
    io.uart1_tx := uart1.io.tx
    io.uart2_tx := uart2.io.tx

    // Connect external RX pins for potential external input
    // when(io.uart1_rx =/= RegNext(io.uart1_rx)) {
    //     uart1.io.rx := io.uart1_rx
    // }
    // when(io.uart2_rx =/= RegNext(io.uart2_rx)) {
    //     uart2.io.rx := io.uart2_rx
    // }

    // Getter methods for accessing internal UARTs
    def getUart1: Uart = uart1
    def getUart2: Uart = uart2
}
