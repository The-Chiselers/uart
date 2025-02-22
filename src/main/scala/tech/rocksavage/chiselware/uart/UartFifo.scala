package tech.rocksavage.chiselware.uart

import chisel3._
import chisel3.util._

/** FIFO implementation for UART TX/RX buffering
  *
  * @param width Data width in bits
  * @param depth FIFO depth (must be power of 2)
  */
class UartFifo(width: Int, depth: Int) extends Module {
    require(isPow2(depth), "FIFO depth must be a power of 2")
    
    val io = IO(new Bundle {
        val write     = Input(Bool())
        val read      = Input(Bool())
        val dataIn    = Input(UInt(width.W))
        val dataOut   = Output(UInt(width.W))
        val full      = Output(Bool())
        val empty     = Output(Bool())
        val count     = Output(UInt(log2Ceil(depth + 1).W))
        val halfFull  = Output(Bool())
    })

    // Memory array to store data
    val mem = RegInit(VecInit(Seq.fill(depth)(0.U(width.W))))
    
    // Pointers and counters
    val writePtr = RegInit(0.U(log2Ceil(depth).W))
    val readPtr  = RegInit(0.U(log2Ceil(depth).W))
    val count    = RegInit(0.U(log2Ceil(depth + 1).W))
    
    // Status flags
    val empty = count === 0.U
    val full  = count === depth.U
    val halfFull = count >= (depth/2).U

    // Debug prints
    when(io.write) {
        printf("[FIFO DEBUG] Write attempt - data: %d, full: %d\n", io.dataIn, full)
    }
    when(io.read) {
        printf("[FIFO DEBUG] Read attempt - empty: %d\n", empty)
    }

    // Write logic
    val validWrite = io.write && !full
    when(validWrite) {
        mem(writePtr) := io.dataIn
        writePtr := Mux(writePtr === (depth-1).U, 0.U, writePtr + 1.U)
        printf("[FIFO DEBUG] Write successful - ptr: %d, data: %d\n", writePtr, io.dataIn)
    }

    // Read logic
    val validRead = io.read && !empty
    when(validRead) {
        readPtr := Mux(readPtr === (depth-1).U, 0.U, readPtr + 1.U)
        printf("[FIFO DEBUG] Read successful - ptr: %d, data: %d\n", readPtr, mem(readPtr))
    }

    // Simplified counter logic
    switch(Cat(validWrite, validRead)) {
        is("b10".U) { count := count + 1.U }  // Write only
        is("b01".U) { count := count - 1.U }  // Read only
        // For "b00" and "b11", count stays the same
    }

    // Debug current state
    when(count =/= RegNext(count)) {
        printf("[FIFO DEBUG] Count changed: %d -> %d\n", RegNext(count), count)
    }

    // Outputs
    io.dataOut  := mem(readPtr)
    io.empty    := empty
    io.full     := full
    io.count    := count
    io.halfFull := halfFull

    // Extra debug for status changes
    when(full =/= RegNext(full)) {
        printf("[FIFO DEBUG] Full status changed: %d -> %d\n", RegNext(full), full)
    }
    when(empty =/= RegNext(empty)) {
        printf("[FIFO DEBUG] Empty status changed: %d -> %d\n", RegNext(empty), empty)
    }
}