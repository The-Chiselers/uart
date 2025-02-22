// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)
package tech.rocksavage.chiselware.uart

import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.uart.bundle.UartTxBundle
import tech.rocksavage.chiselware.uart.param.UartParams
import tech.rocksavage.chiselware.uart.error.UartTxError


/** A UART transmitter module that handles the transmission of UART data.
  * @param params
  *   Configuration parameters for the UART.
  * @param formal
  *   A boolean value to enable formal verification.
  */
class UartTx(params: UartParams, formal: Boolean = true) extends Module {
    val io = IO(new UartTxBundle(params))

    // Add FIFO status signals
    val fifoStatus = IO(new Bundle {
        val full = Output(Bool())
        val empty = Output(Bool())
        val count = Output(UInt(log2Ceil(params.fifoDepth + 1).W))
        val halfFull = Output(Bool())
    })

    // Instantiate TX FIFO
    val txFifo = Module(new UartFifo(params.maxOutputBits, params.fifoDepth))

    // ###################
    // Input Control State Registers
    // ###################
    // These registers hold the configuration settings loaded at the start of a transmission.
    val clocksPerBitReg = RegInit(0.U((log2Ceil(params.maxClocksPerBit) + 1).W))
    val numOutputBitsReg = RegInit(0.U((log2Ceil(params.maxOutputBits) + 1).W))
    val useParityReg     = RegInit(false.B)
    // New registers for parity (nomenclature as in UartRx.scala)
    val parityOddReg = RegInit(false.B)
    val clocksPerBitDbReg = RegInit(
      0.U((log2Ceil(params.maxClocksPerBit) + 1).W)
    )
    val numOutputBitsDbReg = RegInit(
      0.U((log2Ceil(params.maxOutputBits) + 1).W)
    )
    val useParityDbReg = RegInit(false.B)
    val parityOddDbReg = RegInit(false.B) // New parity configuration register

    val clocksPerBitNext = WireInit(
      0.U((log2Ceil(params.maxClocksPerBit) + 1).W)
    )
    val numOutputBitsNext = WireInit(
      0.U((log2Ceil(params.maxOutputBits) + 1).W)
    )
    val useParityNext = WireInit(false.B)
    val parityOddNext = WireInit(false.B) // New NEXT signal for parity odd

    clocksPerBitReg  := clocksPerBitNext
    numOutputBitsReg := numOutputBitsNext
    useParityReg     := useParityNext
    parityOddReg     := parityOddNext // Latch new parity configuration

    clocksPerBitDbReg  := io.txConfig.clocksPerBitDb
    numOutputBitsDbReg := io.txConfig.numOutputBitsDb
    useParityDbReg     := io.txConfig.useParityDb
    parityOddDbReg     := io.txConfig.parityOddDb // load configuration

    // ###################
    // State FSM
    // ###################

    val uartFsm = Module(new UartFsm(params, formal))

    val stateWire        = uartFsm.io.state
    val sampleStartWire  = uartFsm.io.sampleStart
    val sampleDataWire   = uartFsm.io.sampleData
    val sampleParityWire = uartFsm.io.sampleParity
    val completeWire     = uartFsm.io.complete
    val applyShiftReg    = uartFsm.io.shiftRegister
    val txErrorReg = RegInit(UartTxError.None)

    // Store the current transmission data
    val txDataReg = RegInit(0.U(params.maxOutputBits.W))
    when(uartFsm.io.fifoRead) {
        txDataReg := txFifo.io.dataOut
        printf(p"ABOD: loaded new transmission data ${txFifo.io.dataOut}\n")  
    }

    // FSM control
    val startTransaction = !txFifo.io.empty && (uartFsm.io.state === UartState.Idle)
    uartFsm.io.startTransaction := startTransaction
    uartFsm.io.clocksPerBitReg  := clocksPerBitReg
    uartFsm.io.numOutputBitsReg := numOutputBitsReg
    uartFsm.io.useParityReg     := useParityReg

    // Handle error conditions
    when(io.txConfig.load) {
        when(txFifo.io.full) {
            txErrorReg := UartTxError.OverflowError
            printf("[UartTx DEBUG] Setting overflow error - FIFO full\n")
        }
    }.elsewhen(startTransaction && txFifo.io.empty) {
        txErrorReg := UartTxError.UnderflowError
        printf("[UartTx DEBUG] Setting underflow error - FIFO empty\n")
    }.elsewhen(uartFsm.io.complete) {
        txErrorReg := UartTxError.None
        printf("[UartTx DEBUG] Clearing error after completion\n")
    }

    // Debug status changes
    when(txErrorReg =/= RegNext(txErrorReg)) {
        printf("[UartTx DEBUG] Error state changed: from %d to %d\n", 
            RegNext(txErrorReg).asUInt, txErrorReg.asUInt)
    }

    // Error output
    io.error := txErrorReg

    // Connect FIFO with debug
    txFifo.io.write := io.txConfig.load && !txFifo.io.full
    txFifo.io.read := uartFsm.io.fifoRead
    txFifo.io.dataIn := io.txConfig.data
    
    // ###################
    // Shift Register for Storing Data to Transmit
    // ###################
    // In the idle state, when a new transmission is requested (via io.load), the transmit data is loaded.
    val dataShiftReg  = RegInit(0.U(params.maxOutputBits.W))
    val dataShiftNext = WireInit(0.U(params.maxOutputBits.W))
    dataShiftReg := dataShiftNext

    // Add after dataShiftReg assignment
when(dataShiftReg =/= RegNext(dataShiftReg)) {
    printf(p"[UartTx DEBUG] Shift register changed: ${RegNext(dataShiftReg)} -> ${dataShiftReg}\n")
}

    // ###################
    // Output Register
    // ###################
    // The TX output should be high in idle.
    val txNext = WireInit(true.B)
    // The calculateTxOut now also has a branch for the Parity state.
    txNext := calculateTxOut(stateWire, dataShiftReg, txDataReg, parityOddReg)
    io.tx  := txNext

    // Connect FIFO status outputs
    fifoStatus.full := txFifo.io.full
    fifoStatus.empty := txFifo.io.empty
    fifoStatus.count := txFifo.io.count
    fifoStatus.halfFull := txFifo.io.halfFull


    // ###################
    // Finite State Machine (FSM)
    // ###################
    // Updated state transitions to include parity mode.

    clocksPerBitNext := Mux(
      stateWire === UartState.Idle,
      clocksPerBitDbReg,
      clocksPerBitReg
    )

    numOutputBitsNext := Mux(
      stateWire === UartState.Idle,
      numOutputBitsDbReg,
      numOutputBitsReg
    )

    useParityNext := Mux(
      stateWire === UartState.Idle,
      useParityDbReg,
      useParityReg
    )

    parityOddNext := Mux(
      stateWire === UartState.Idle,
      parityOddDbReg,
      parityOddReg
    )

    dataShiftNext := calculateDataShiftNext(
      dataShiftReg,
      txFifo.io.dataOut,
      startTransaction,
      applyShiftReg
    )

    // -------------------------
    // FSM and Data Handling Functions
    // -------------------------

    def calculateDataShiftNext(
        dataShiftReg: UInt,
        loadData: UInt,
        startTransaction: Bool,
        applyShiftReg: Bool
    ): UInt = {
        val dataShiftNext = WireDefault(dataShiftReg)

        // Priority load: if we are starting a new transaction,
        // we do NOT also shift in this cycle.
        when (startTransaction) {
          dataShiftNext := Reverse(loadData)
        } .elsewhen (applyShiftReg) {
          dataShiftNext := Cat(0.U(1.W), dataShiftReg >> 1)
        }

        dataShiftNext
    }

    // The TX output is driven based on the current FSM state.
    // For parity state we compute the parity bit from the full (latched) data.
    def calculateTxOut(
      stateReg: UartState.Type,
      dataShiftReg: UInt,
      txData: UInt,
      parityOddReg: Bool
  ): Bool = {
    val txOut = WireInit(true.B)
    switch(stateReg) {
      is(UartState.Idle)   { txOut := true.B }
      is(UartState.Load)   { txOut := true.B } // During Load, remain idle.
      is(UartState.Start)  { txOut := false.B }
      is(UartState.Data)   { txOut := dataShiftReg(0) }
      is(UartState.Parity) { txOut := UartParity.parityChisel(txData, parityOddReg) }
      is(UartState.Stop)   { txOut := true.B }
    }
    txOut
  }


  // Debug signals - when verbose mode is enabled
  when(params.verbose.B) {
      when(io.txConfig.load) {
          printf(
              p"[UartTx.scala DEBUG] Loading data: ${io.txConfig.data}, FIFO count: ${txFifo.io.count}\n"
          )
      }
      when(startTransaction) {
          printf(
              p"[UartTx.scala DEBUG] Starting transmission, data: ${txDataReg}\n"
          )
      }
  }

}
