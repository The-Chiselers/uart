package tech.rocksavage.chiselware.uart

import chisel3._
import chiseltest._
import tech.rocksavage.chiselware.uart.param.UartParams

object parityTests {
    def rxOddParityTest(dut: UartRx, params: UartParams): Unit = {
        implicit val clock = dut.clock
        dut.clock.setTimeout(2000)
        val clocksPerBit  = 217
        val numOutputBits = 8

        // Configure for odd parity
        dut.io.rxConfig.clocksPerBitDb.poke(clocksPerBit.U)
        dut.io.rxConfig.numOutputBitsDb.poke(numOutputBits.U)
        dut.io.rxConfig.useParityDb.poke(true.B)
        dut.io.rxConfig.parityOddDb.poke(true.B)
        dut.clock.step()

        val data: Int = 65
        val dataBits =
            (0 until numOutputBits).map(i => ((data >> i) & 1) == 1).reverse
        val expectedParity = true // computed as 1

        // Build the sequence: start (0), 8 data bits, parity bit, stop (1)
        val expectedSequence =
            Seq(false) ++ dataBits ++ Seq(expectedParity, true)

        // Transmit each bit for one bit-period
        for (bit <- expectedSequence) {
            dut.io.rx.poke(bit.B)
            dut.clock.setTimeout(clocksPerBit + 1)
            dut.clock.step(clocksPerBit)
        }

        // After stop bit, data output should be available
        dut.io.data.expect(data.U)
    }

    def txOddParityTest(dut: UartTx, params: UartParams): Unit = {
    implicit val clock = dut.clock
    dut.clock.setTimeout(0)
    val clocksPerBit = 217
    val numOutputBits = 8

    // Configure TX for even parity
    dut.io.txConfig.clocksPerBitDb.poke(clocksPerBit.U)
    dut.io.txConfig.numOutputBitsDb.poke(numOutputBits.U)
    dut.io.txConfig.useParityDb.poke(true.B)
    dut.io.txConfig.parityOddDb.poke(false.B)
    dut.clock.step(10)

    // Before loading transmission, TX should be idle high
    dut.io.tx.expect(true.B)

    val rawData: Int = 65  // ASCII 'A' = 0x41 = 0b01000001
    val paddedData = rawData & 0xFF

    // The shift register reverses the bits for transmission
    // so we need to calculate based on the reversed bits
    val reversedData = {
        var result = 0
        for (i <- 0 until 8) {
            if ((paddedData & (1 << i)) != 0) {
                result |= (1 << (7 - i))
            }
        }
        result
    }
    println(f"Reversed data: 0x$reversedData%02X")

    val dataBits = for (i <- 0 until 8) yield {
        val bit = (reversedData >> i) & 1
        println(s"Bit $i: ${bit}")
        bit == 1
    }
    
    // Count 1s for parity calculation
    val numOnes = dataBits.count(identity)
    println(s"Number of 1s in reversed data: $numOnes")
    
    // For even parity:
    // If numOnes is even, parity should be 0 to keep total even
    // If numOnes is odd, parity should be 1 to make total even
    val expectedParity = numOnes % 2 != 0
    println(s"Expected parity bit: $expectedParity (to maintain even total)")

    // Build expected sequence: start (0), data bits, parity, stop (1)
    val expectedSequence = Seq(false) ++ dataBits ++ Seq(expectedParity, true)
    println("Expected sequence: " + expectedSequence.map(if (_) "1" else "0").mkString)

    // Initiate transmission
    dut.io.txConfig.data.poke(rawData.U)
    dut.io.txConfig.load.poke(true.B)
    dut.clock.step()
    dut.io.txConfig.load.poke(false.B)

    // Wait for transmission to start (TX to go low)
    var timeout = 0
    while (dut.io.tx.peek().litToBoolean && timeout < clocksPerBit * 2) {
        dut.clock.step(1)
        timeout += 1
    }
    println(s"Start bit detected after $timeout cycles")

    assert(timeout < clocksPerBit * 2, "Timeout waiting for start bit")

    def waitAndCheckBit(expected: Boolean): Unit = {
        // Wait some cycles, then check midpoint
        dut.clock.step(clocksPerBit / 2)
        dut.io.tx.expect(expected.B)
        // Continue to next bit
        dut.clock.step(clocksPerBit / 2)
    }

    // Check start bit (we've already found its beginning)
    waitAndCheckBit(false)

    // Check each data bit, parity bit, and stop bit
    for (i <- 0 until 8) {
        waitAndCheckBit(dataBits(i))
    }
    waitAndCheckBit(expectedParity)
    waitAndCheckBit(true)

    // Verify return to idle
    dut.io.tx.expect(true.B)
    println("Transmission complete, verified return to idle")
}
}
