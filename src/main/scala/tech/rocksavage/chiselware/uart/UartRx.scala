// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)
package tech.rocksavage.chiselware.uart

import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.uart.bundle.UartRxBundle
import tech.rocksavage.chiselware.uart.error.UartRxError
import tech.rocksavage.chiselware.uart.param.UartParams

/** A UART receiver module that handles the reception of UART data.
  *
  * @param params
  *   Configuration parameters for the UART.
  * @param formal
  *   A boolean value to enable formal verification.
  */
class UartRx(params: UartParams, formal: Boolean = true) extends Module {

    /** Input/Output interface for the UART receiver */
    val io = IO(new UartRxBundle(params))

    // Add FIFO status signals to the bundle
    val fifoStatus = IO(new Bundle {
        val full = Output(Bool())
        val empty = Output(Bool())
        val count = Output(UInt(log2Ceil(params.fifoDepth + 1).W))
        val halfFull = Output(Bool())
    })

    // Instantiate the RX FIFO
    val rxFifo = Module(new UartFifo(params.maxOutputBits, params.fifoDepth))

    // ###################
    // RX Input Synchronization
    // ###################

    /** Shift register for synchronizing received data */
    val rxSyncRegs = RegInit(VecInit(Seq.fill(params.syncDepth)(true.B)).asUInt)

    /** Next shift register for synchronizing received data */
    val rxSyncNext = WireInit(0.U(params.syncDepth.W))

    rxSyncRegs := rxSyncNext

    /** Current synchronized rx input */
    val rxSync = Wire(Bool())
    rxSync := rxSyncRegs(params.syncDepth - 1)

    // ###################
    // Input Control State Registers
    // ###################

    /** Clocks per bit register */
    val clocksPerBitReg = RegInit(0.U((log2Ceil(params.maxClocksPerBit) + 1).W))

    /** Number of output bits register */
    val numOutputBitsReg = RegInit(0.U((log2Ceil(params.maxOutputBits) + 1).W))

    /** Parity usage register */
    val useParityReg = RegInit(false.B)
    val parityOddReg = RegInit(false.B)

    val clearErrorReg = RegInit(false.B)

    val clocksPerBitDbReg = RegInit(
      0.U((log2Ceil(params.maxClocksPerBit) + 1).W)
    )

    val numOutputBitsDbReg = RegInit(
      0.U((log2Ceil(params.maxOutputBits) + 1).W)
    )

    val useParityDbReg = RegInit(false.B)
    val parityOddDbReg = RegInit(false.B)

    val clearErrorDbReg = RegInit(false.B)

    /** Next Clocks per bit register */
    val clocksPerBitNext = WireInit(
      0.U((log2Ceil(params.maxClocksPerBit) + 1).W)
    )

    /** Next Number of output bits register */
    val numOutputBitsNext = WireInit(
      0.U((log2Ceil(params.maxOutputBits) + 1).W)
    )

    /** Next Parity usage register */
    val useParityNext = WireInit(false.B)
    val parityOddNext = WireInit(false.B)

    val clearErrorNext = WireInit(false.B)

    clocksPerBitReg  := clocksPerBitNext
    numOutputBitsReg := numOutputBitsNext
    useParityReg     := useParityNext
    parityOddReg     := parityOddNext
    clearErrorReg    := clearErrorNext

    clocksPerBitDbReg  := io.rxConfig.clocksPerBitDb
    numOutputBitsDbReg := io.rxConfig.numOutputBitsDb
    useParityDbReg     := io.rxConfig.useParityDb
    parityOddDbReg     := io.rxConfig.parityOddDb
    clearErrorDbReg    := io.rxConfig.clearErrorDb

    // ###################
    // State FSM
    // ###################

    val uartFsm = Module(new UartFsm(params, formal))

    val stateWire        = uartFsm.io.state
    val sampleStartWire  = uartFsm.io.sampleStart
    val sampleDataWire   = uartFsm.io.sampleData
    val sampleParityWire = uartFsm.io.sampleParity
    val completeWire     = uartFsm.io.complete

    val startTransaction =
        (rxSync === false.B) && (stateWire === UartState.Idle)

    uartFsm.io.startTransaction := startTransaction
    uartFsm.io.clocksPerBitReg  := clocksPerBitReg
    uartFsm.io.numOutputBitsReg := numOutputBitsReg
    uartFsm.io.useParityReg     := useParityReg

    // ###################
    // Shift Register for Storing Received Data
    // ###################

    /** Shift register for storing received data */
    val dataShiftReg = RegInit(0.U(params.maxOutputBits.W))

    /** Next shift register for storing received data */
    val dataShiftNext = WireInit(0.U(params.maxOutputBits.W))

    dataShiftReg := dataShiftNext

    // ###################
    // Output Registers
    // ###################

    /** Data output register */
    val dataReg = RegInit(0.U(params.maxOutputBits.W))

    /** Valid output register */
    val validReg = RegInit(false.B)

    /** Error output register */
    val errorReg = RegInit(UartRxError.None)

    /** Next Data output register */
    val dataNext = WireInit(0.U(params.maxOutputBits.W))

    /** Next Valid output register */
    val validNext = WireInit(false.B)

    /** Next Error output register */
    val errorNext = WireInit(UartRxError.None)

    dataReg  := dataNext
    validReg := validNext
    errorReg := errorNext

    // FIFO write control
    val fifoWriteEnable = Wire(Bool())
    fifoWriteEnable := uartFsm.io.complete && !rxFifo.io.full

    // Connect FIFO
    rxFifo.io.write := fifoWriteEnable
    rxFifo.io.read := io.read  // New signal needed in UartRxBundle
    rxFifo.io.dataIn := dataShiftReg
    io.data := rxFifo.io.dataOut

    // Connect FIFO status outputs
    fifoStatus.full := rxFifo.io.full
    fifoStatus.empty := rxFifo.io.empty
    fifoStatus.count := rxFifo.io.count
    fifoStatus.halfFull := rxFifo.io.halfFull

    // Valid signal now depends on FIFO not being empty
    io.valid := !rxFifo.io.empty

    // ###################
    // Output Assignments
    // ###################

    /** Assign output data */
    //io.data := dataReg

    /** Assign output valid signal */
    //io.valid := validReg

    /** Assign output error signal */
    io.error := errorReg // Ensure io.error is always assigned

    // ###################
    // Finite State Machine (FSM)
    // ###################

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

    clearErrorNext := Mux(
      stateWire === UartState.Idle,
      clearErrorDbReg,
      clearErrorReg
    )

    rxSyncNext := calculateRxSyncNext(
      rxSyncRegs,
      io.rx
    )

    dataShiftNext := calculateDataShiftNext(
      rxSync,
      dataShiftReg,
      sampleDataWire,
      completeWire
    )

    dataNext := calculateDataNext(
      dataShiftReg,
      dataReg,
      completeWire
    )

    validNext := calculateValidNext(
      validReg,
      completeWire,
      startTransaction
    )

    errorNext := calculateErrorNext(
      stateWire,
      rxSync,
      uartFsm.io.clockCounterOut,
      clocksPerBitReg
    )

    /** Computes the next rxSync value.
      *
      * @return
      *   The next rxSync value.
      */
    def calculateRxSyncNext(
        rxSyncRegs: UInt,
        rx: Bool
    ): UInt = {
        val rxSyncNext = WireInit(0.U(params.syncDepth.W))
        rxSyncNext := Cat(rxSyncRegs(params.syncDepth - 2, 0), rx)
        rxSyncNext
    }

    /** Computes the next dataShift value.
      *
      * @return
      *   The next dataShift value.
      */
    def calculateDataShiftNext(
        rxSync: Bool,
        dataShiftReg: UInt,
        sampleWire: Bool,
        completeWire: Bool
    ): UInt = {
        val dataShiftNext = WireDefault(dataShiftReg)

        when(sampleWire) {
            dataShiftNext := Cat(
              dataShiftReg(params.maxOutputBits - 2, 0),
              rxSync
            )
        }
        when(completeWire) {
            dataShiftNext := 0.U
        }
        dataShiftNext
    }

    /** Computes the next data value.
      *
      * @return
      *   The next data value.
      */
    def calculateDataNext(
        dataShiftReg: UInt,
        dataReg: UInt,
        completeWire: Bool
    ): UInt = {
        val dataNext = WireDefault(dataReg)
        when(completeWire) {
            dataNext := dataShiftReg
        }
        dataNext
    }

    /** Computes the next valid value.
      *
      * @return
      *   The next valid value.
      */
    def calculateValidNext(
        validReg: Bool,
        completeWire: Bool,
        startTransaction: Bool
    ): Bool = {
        val validNext = WireDefault(validReg)
        when(completeWire) {
            validNext := true.B
        }
        when(startTransaction) {
            validNext := false.B
        }
        validNext
    }

    /** Computes the next error value.
      *
      * @return
      *   The next error value.
      */
     def calculateErrorNext(
    stateReg: UartState.Type,
    rxSync: Bool,
    clockCounterReg: UInt,
    clocksPerBitReg: UInt
): UartRxError.Type = {
    val errorNext = WireDefault(errorReg)

    when(io.rxConfig.clearErrorDb) {
        when(stateReg === UartState.Idle) {
            // Only clear errors when in idle state
            errorNext := UartRxError.None
            printf("[UartRx DEBUG] Clearing error in IDLE state\n")
        }
    }.otherwise {
        switch(stateReg) {
            is(UartState.Start) {
                // Start bit should stay low for the entire bit time
                when(rxSync === true.B) {
                    errorNext := UartRxError.StartBitError
                    printf("[UartRx DEBUG] StartBitError: rx went high during start bit at counter=%d\n", 
                           clockCounterReg)
                }
            }
            is(UartState.Stop) {
              when(clockCounterReg === clocksPerBitReg && rxSync =/= true.B) {
                printf(p"[UartRx.scala DEBUG] StopBitError detected! rxSync=$rxSync\n")
                errorNext := UartRxError.StopBitError
              }
            }
            is(UartState.Parity) {
                when(clockCounterReg === clocksPerBitReg) {
                    val dataOnes = PopCount(dataShiftReg)
                    val dataParity = dataOnes(0)
                    val expected = Mux(parityOddReg, dataParity, ~dataParity)
                    when(rxSync =/= expected) {
                        errorNext := UartRxError.ParityError
                        printf("[UartRx DEBUG] ParityError detected\n")
                    }
                }
            }
        }
    }

    when(errorNext =/= errorReg) {
        printf("[UartRx DEBUG] Error changed: %d -> %d\n", errorReg.asUInt, errorNext.asUInt)
    }

    // Extra debug
    when(stateReg === UartState.Start) {
        printf("[UartRx DEBUG] Start bit monitoring: counter=%d, rxSync=%d\n", 
               clockCounterReg, rxSync)
    }

    errorNext
}
}
