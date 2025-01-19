// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)

package tech.rocksavage.chiselware.uart

import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.uart.bundle.UartRxBundle
import tech.rocksavage.chiselware.uart.error.UartRxError
import tech.rocksavage.chiselware.uart.param.UartParams

/* This code is licensed under the Apache Software License 2.0 (see LICENSE.MD
//package tech.rocksavage.chiselware.uart.bundle
//
//import chisel3._
//import chisel3.util._
//import tech.rocksavage.chiselware.uart.param.UartParams
//
//// from the perspective of the UART which is receiving data
//class UartRxBundle(params: UartParams) extends Bundle {
//  val rx = Input(Bool())
//  val data = Input(UInt(params.dataWidth.W))
//  val valid = Output(Bool())
//  val ready = Input(Bool())
//
//  val outputBits = Output(UInt(params.maxOutputBits.W))
//  val outputValid = Output(Bool())
//
//  // configuration inputs
//
//  // clock = 25 MHz
//  // baud = 115200
//  // 25000000 / 115200 = 217 Clocks Per Bit.
//  val clocksPerBitDb = Input(UInt(log2Ceil(params.maxClocksPerBit).W))
//  val numOutputBitsDb = Input(UInt(log2Ceil(params.maxOutputBits).W))
//  val useParityDb = Input(Bool())
//} */

class UartRx(params: UartParams) extends Module {
  val io = IO(new UartRxBundle(params))

  // Internal Registers
  val stateReg     = RegInit(UartState.Idle)
  val bitCounter   = RegInit(0.U(log2Ceil(params.dataWidth).W))
  val clockCounter = RegInit(0.U(log2Ceil(params.maxClocksPerBit).W))
  val dbUpdate     = WireInit(stateReg === UartState.Idle)

  // Input Control State Registers /
  val clocksPerBitReg  = RegInit(0.U(log2Ceil(params.maxClocksPerBit).W))
  val numOutputBitsReg = RegInit(0.U(log2Ceil(params.maxOutputBits).W))
  val useParityReg     = RegInit(false.B)

  // Input Sync
  val rxSyncRegs = RegInit(0.U(params.syncDepth.W))
  val rxSync     = rxSyncRegs(params.syncDepth - 1)

  // Shift register for storing received data
  val shiftReg = RegInit(0.U(params.dataWidth.W))

  // Output registers
  val dataReg  = RegInit(0.U(params.dataWidth.W))
  val validReg = RegInit(false.B)
  val errorReg = RegInit(UartRxError.None)

  // Output
  io.rx.data := dataReg
  io.rx.valid := validReg



  // FSM
  switch(stateReg) {
    is(UartState.Idle) {
      when(io.rx.rxtx === false.B) {
        stateReg     := UartState.Start
        bitCounter   := 0.U
        clockCounter := 0.U
      }
    }
    is(UartState.Start) {
      when(io.rx.rxtx === true.B) {
        stateReg := UartState.Idle
        errorReg := UartRxError.StartBitError
      }.otherwise {
        stateReg := UartState.Data
      }
    }
    is(UartState.Data) {
      when(clockCounter === clocksPerBitReg) {
        when(bitCounter === (numOutputBitsReg - 1.U)) {
          stateReg := UartState.Stop
        }.otherwise {
          bitCounter   := bitCounter + 1.U
          clockCounter := 0.U
        }
      }.otherwise {
        clockCounter := clockCounter + 1.U
      }
    }
    is(UartState.Stop) {
      when(rxSync === true.B) {
        stateReg := UartState.Idle
        errorReg := UartRxError.StopBitError
      }.otherwise {
        stateReg := UartState.Idle
        dataReg  := shiftReg

        validReg := true.B
      }
    }
  }

  // Update the shift register
  when(stateReg === UartState.Data) {
    shiftReg := Cat(shiftReg(params.dataWidth - 2, 0), rxSync)
  }.otherwise {
    shiftReg := shiftReg
  }

  // update the sync registers
  rxSyncRegs := Cat(rxSyncRegs(params.syncDepth - 2, 0), io.rx.rxtx)

  // Update the internal registers
  when(dbUpdate) {
    clocksPerBitReg  := io.clocksPerBitDb
    numOutputBitsReg := io.numOutputBitsDb
    useParityReg     := io.useParityDb
  }.otherwise({
    clocksPerBitReg  := clocksPerBitReg
    numOutputBitsReg := numOutputBitsReg
    useParityReg     := useParityReg
  })

}
