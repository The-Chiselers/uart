// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)
package tech.rocksavage.chiselware.uart

import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.dynamicfifo.{DynamicFifo, DynamicFifoParams}
import tech.rocksavage.chiselware.uart.UartUtils.reverse
import tech.rocksavage.chiselware.uart.bundle.UartTxBundle
import tech.rocksavage.chiselware.uart.error.UartTxError
import tech.rocksavage.chiselware.uart.param.UartParams

/** A UART transmitter module that handles the transmission of UART data.
  * @param params
  *   Configuration parameters for the UART.
  * @param formal
  *   A boolean value to enable formal verification.
  */
class UartTx(params: UartParams, formal: Boolean = true) extends Module {

    val io = IO(new UartTxBundle(params))

    when(io.txConfig.load) {
        printf(
          "[UartTx DEBUG] Loading data: %d\n",
          io.txConfig.data.asUInt
        )
    }

    // ###################
    // Input Control State Registers
    // ###################
    // These registers hold the configuration settings loaded at the start of a transmission.
    val clocksPerBitReg = RegInit(
      0.U((log2Ceil(params.maxClockFrequency) + 1).W)
    )
    val numOutputBitsReg = RegInit(0.U((log2Ceil(params.maxOutputBits) + 1).W))
    val useParityReg     = RegInit(false.B)
    val parityOddReg     = RegInit(false.B)

    val numOutputBitsDbReg = RegInit(
      0.U((log2Ceil(params.maxOutputBits) + 1).W)
    )
    val useParityDbReg = RegInit(false.B)
    val parityOddDbReg = RegInit(false.B) // New parity configuration register

    val numOutputBitsNext = WireInit(
      0.U((log2Ceil(params.maxOutputBits) + 1).W)
    )
    val useParityNext = WireInit(false.B)
    val parityOddNext = WireInit(false.B) // New NEXT signal for parity odd
    val dataNext      = WireInit(0.U(params.maxOutputBits.W))
    val loadNext      = WireInit(false.B)

    val clearErrorReg   = RegInit(false.B)
    val clearErrorDbReg = RegInit(false.B)
    val clearErrorNext  = WireInit(false.B)
    clearErrorReg   := clearErrorNext
    clearErrorDbReg := io.txConfig.clearErrorDb

    val lsbFirstReg  = RegInit(false.B)
    val lsbFirstNext = WireInit(false.B)
    lsbFirstReg := lsbFirstNext

    val fifoEmptyReg = RegInit(true.B)

    numOutputBitsReg := numOutputBitsNext
    useParityReg     := useParityNext
    parityOddReg     := parityOddNext // Latch new parity configuration

    numOutputBitsDbReg := io.txConfig.numOutputBitsDb
    useParityDbReg     := io.txConfig.useParityDb
    parityOddDbReg     := io.txConfig.parityOddDb // load configuration

    // ###################
    // State FSM
    // ###################

    val uartFsm = Module(new UartFsm(params))

    // false for TX so it transmits at the start of the bit
    uartFsm.io.shiftOffset := false.B

    val state         = uartFsm.io.state
    val shiftStart    = uartFsm.io.shift && state === UartState.Start
    val shiftData     = uartFsm.io.shift && state === UartState.Data
    val shiftParity   = uartFsm.io.shift && state === UartState.Parity
    val completeWire  = uartFsm.io.complete
    val applyShiftReg = shiftData || shiftParity
    val txErrorReg    = RegInit(UartTxError.None)

    val emptywire = WireInit(false.B)

    val startTransaction = WireInit(false.B)
    startTransaction := (RegNext(
      state
    ) === UartState.Idle) && (loadNext)

    uartFsm.io.waiting          := !emptywire
    uartFsm.io.startTransaction := startTransaction
    uartFsm.io.clocksPerBit     := clocksPerBitReg
    uartFsm.io.numOutputBits    := numOutputBitsReg
    uartFsm.io.useParity        := useParityReg
    uartFsm.io.updateBaud       := io.txConfig.updateBaud

    // ###################
    // Baud Rate Calculation
    // ###################

    val baudGen = Module(new UartBaudRateGenerator(params))
    baudGen.io.clkFreq     := io.txConfig.clockFreq
    baudGen.io.desiredBaud := io.txConfig.baud
    baudGen.io.update      := RegNext(state) === UartState.BaudUpdating

    // The effective clocks-per-bit for the UART will come from the baud generator.
    val effectiveClocksPerBit = baudGen.io.clocksPerBit

    // Once the baud rate is valid, the state can go back to idle.
    uartFsm.io.baudValid := baudGen.io.valid

    clocksPerBitReg := effectiveClocksPerBit

    // Latch incoming data and capture txData for parity calculation.
    // (txDataReg holds the unshifted data for parity computation.)
    val txParityData = RegInit(0.U(params.maxOutputBits.W))
    dataNext := io.txConfig.data
    loadNext := io.txConfig.load

    // ###################
    // Shift Register for Storing Data to Transmit
    // ###################
    // In the idle state, when a new transmission is requested (via io.load), the transmit data is loaded.
    val dataShiftReg  = RegInit(0.U(params.maxOutputBits.W))
    val dataShiftNext = WireInit(0.U(params.maxOutputBits.W))
    dataShiftReg := dataShiftNext

    // ###################
    // Fifo for reads
    // ###################
    val fifoParams = DynamicFifoParams(
      dataWidth = params.maxOutputBits,
      fifoDepth = params.bufferSize,
      coverage = false,
      formal = formal
    )
    val fifo = Module(new DynamicFifo(fifoParams))
    io.fifoBundle.full        := fifo.io.full
    io.fifoBundle.empty       := fifo.io.empty
    io.fifoBundle.almostFull  := fifo.io.almostFull
    io.fifoBundle.almostEmpty := fifo.io.almostEmpty

    // Debug status changes
    when(txErrorReg =/= RegNext(txErrorReg)) {
        printf(
          "[UartTx DEBUG] Error state changed: from %d to %d\n",
          RegNext(txErrorReg).asUInt,
          txErrorReg.asUInt
        )
    }

    when(clearErrorDbReg) {
        // When clearErrorDb is set, reset
        txErrorReg := UartTxError.None
    }
    // Error Output
    io.error := txErrorReg

    fifo.io.push   := RegNext(io.txConfig.txDataRegWrite)
    fifo.io.pop    := uartFsm.io.nextTransaction
    fifo.io.dataIn := io.txConfig.data

    fifo.io.almostFullLevel  := 0.U
    fifo.io.almostEmptyLevel := 0.U
    fifoEmptyReg             := fifo.io.empty

    when(uartFsm.io.nextTransaction) {
        txParityData := fifo.io.dataOut
    }

    when(fifo.io.full && fifo.io.push) {
        txErrorReg := UartTxError.FifoOverflow
        printf("[UartTx DEBUG] Setting overflow error - FIFO full\n")
    }.elsewhen(fifo.io.empty && fifo.io.pop) {
        txErrorReg := UartTxError.FifoUnderflow
        printf("[UartTx DEBUG] Setting underflow error - FIFO empty\n")
    }.elsewhen(io.txConfig.clearErrorDb) {
        txErrorReg := UartTxError.None
        printf("[UartTx DEBUG] Clearing error after completion\n")
    }

    // ###################
    // Output Register
    // ###################
    // The TX output should be high in idle.
    val txNext = WireInit(true.B)
    // The calculateTxOut now also has a branch for the Parity state.
    txNext := calculateTxOut(state, dataShiftReg, txParityData, parityOddReg)
    io.tx  := txNext

    io.clocksPerBit := clocksPerBitReg

    // ###################
    // Finite State Machine (FSM)
    // ###################
    // Updated state transitions to include parity mode.

    numOutputBitsNext := Mux(
      state === UartState.Idle || state === UartState.BaudUpdating,
      numOutputBitsDbReg,
      numOutputBitsReg
    )

    useParityNext := Mux(
      state === UartState.Idle || state === UartState.BaudUpdating,
      useParityDbReg,
      useParityReg
    )

    parityOddNext := Mux(
      state === UartState.Idle || state === UartState.BaudUpdating,
      parityOddDbReg,
      parityOddReg
    )

    lsbFirstNext := Mux(
      state === UartState.Idle || state === UartState.BaudUpdating,
      io.txConfig.lsbFirst,
      lsbFirstReg
    )

    dataShiftNext := calculateDataShiftNext(
      dataShiftReg = dataShiftReg,
      loadData = fifo.io.dataOut,
      nextTransaction = uartFsm.io.nextTransaction,
      applyShiftReg = applyShiftReg,
      lsbFirst = lsbFirstNext,
      numOutputBits = numOutputBitsReg
    )

    // -------------------------
    // FSM and Data Handling Functions
    // -------------------------

    def calculateDataShiftNext(
        dataShiftReg: UInt,
        loadData: UInt,
        nextTransaction: Bool,
        applyShiftReg: Bool,
        lsbFirst: Bool,
        numOutputBits: UInt
    ): UInt = {
        val dataShiftNext = WireDefault(dataShiftReg)

        when(nextTransaction) {
            when(!lsbFirst) {
                dataShiftNext := reverse(loadData, numOutputBits)
            }.otherwise {
                dataShiftNext := loadData
            }
        }
        when(applyShiftReg) {
            dataShiftNext := Cat(0.U(1.W), dataShiftReg >> 1)
        }
        dataShiftNext
    }

    // The TX output is driven based on the current FSM state.
    // For parity state we compute the parity bit from the full (latched) data.
    def calculateTxOut(
        stateReg: UartState.Type,
        dataShiftNext: UInt,
        txData: UInt,
        parityOddReg: Bool
    ): Bool = {
        val txOut = WireInit(true.B)
        switch(stateReg) {
            is(UartState.Idle) {
                txOut := true.B
            }
            is(UartState.BaudUpdating) {
                txOut := true.B
            }
            is(UartState.Start) {
                txOut := false.B
            }
            is(UartState.Data) {
                txOut := dataShiftNext(0)
            }
            is(UartState.Parity) {
                txOut := UartParity.parityChisel(txData, parityOddReg)
            }
            is(UartState.Stop) {
                txOut := true.B
            }
        }
        txOut
    }
}
