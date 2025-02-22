// (c) 2024 Rocksavage Technology, Inc.
// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)

package tech.rocksavage.chiselware.uart

import chisel3._
import chiseltest._

object UartTestUtils {
    def transactionChar(dut: UartRx, char: Char, clocksPerBit: Int): Unit = {
//    val binString = char.toBinaryString.padTo(8, '0').reverse
        val binString = char.toBinaryString.reverse.padTo(8, '0').reverse
        val bits      = binString.map(_ == '1')
        println(s"Transmitting character: $char, with bits: ${binString}")
        transaction(dut, bits, clocksPerBit, 8)
    }

    def transaction(
        dut: UartRx,
        bits: Seq[Boolean],
        clocksPerBit: Int,
        numBits: Int
    ): Unit = {

        // Transmit the start bit
        println("--- Transmitting start bit")
        transmitBit(dut, false, clocksPerBit)
        // Transmit numBits bits
        println("--- Transmitting data bits")
        for (i <- 0 until numBits) {
            transmitBit(dut, bits(i), clocksPerBit)
        }
        // Transmit the stop bit
        println("--- Transmitting stop bit")
        UartTestUtils.transmitBit(dut, true, clocksPerBit)
    }

    def transmitBit(dut: UartRx, bit: Boolean, clocksPerBit: Int): Unit = {
        if (bit) println("--- --- 1")
        else
            println("--- --- 0")
        dut.io.rx.poke(bit.B)

        println(s"Transmitting bit for $clocksPerBit cycles")
        // dut.clock.setTimeout(clocksPerBit + 1)
        for (i <- 0 until clocksPerBit) {
            dut.clock.step()
        }
    }
}
