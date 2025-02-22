// transmissionTests.scala
package tech.rocksavage.chiselware.uart

import chisel3._
import chiseltest._
import tech.rocksavage.chiselware.uart.param.UartParams
object transmissionTests {
    def basicRxTest(dut: UartRx, params: UartParams): Unit = {
        implicit val clock = dut.clock

        val clocksPerBit = 217
        val numOutputBits = 8

        // Reset the device
        dut.io.rx.poke(1.U)
        dut.io.rxConfig.clocksPerBitDb.poke(clocksPerBit.U)
        dut.io.rxConfig.numOutputBitsDb.poke(numOutputBits.U)
        dut.io.rxConfig.useParityDb.poke(false.B)
        dut.io.rxConfig.parityOddDb.poke(false.B)
        
        // Initialize read signal to false
        dut.io.read.poke(false.B)
        
        // Initial stabilization delay
        dut.clock.step(10)

        val chars = Seq('s', 'B', 'C', 'D', 'E', 'F', 'G', 'H')
        for (char <- chars) {
            println(s"Testing character: $char")
            
            // Send the character
            UartTestUtils.transactionChar(dut, char, clocksPerBit)

            // Wait for valid signal (FIFO not empty)
            var timeout = 0
            dut.clock.setTimeout(20000)
            
            while (!dut.io.valid.peek().litToBoolean && timeout < clocksPerBit * 15) {
                dut.clock.step(1)
                timeout += 1
            }

            if (timeout >= clocksPerBit * 15) {
                throw new RuntimeException(s"Timeout waiting for valid signal on character $char")
            }

            // Read the data before asserting read
            val currentData = dut.io.data.peek().litValue
            println(s"Found data in FIFO: $currentData")

            // Verify data is correct before reading
            dut.io.data.expect(char.U)

            // Now do the FIFO read - one cycle pulse
            dut.io.read.poke(true.B)
            dut.clock.step(1)
            dut.io.read.poke(false.B)

            // Wait for any FIFO operations to complete
            dut.clock.step(5)

            // Extra delay between characters
            dut.clock.step(clocksPerBit)
        }
    }
    def basicTxTest(dut: UartTx, params: UartParams): Unit = {
    implicit val clock = dut.clock
    dut.clock.setTimeout(0)

    val clocksPerBit = 217
    val numOutputBits = 8

    // Drive the configuration inputs
    dut.io.txConfig.clocksPerBitDb.poke(clocksPerBit.U)
    dut.io.txConfig.numOutputBitsDb.poke(numOutputBits.U)
    dut.io.txConfig.useParityDb.poke(false.B)
    dut.io.txConfig.parityOddDb.poke(false.B)

    // Initial stabilization
    dut.clock.step(10)
    dut.io.tx.expect(true.B)

    def waitAndCheckBit(expected: Boolean): Unit = {
        // Wait some cycles, then check midpoint
        dut.clock.step(clocksPerBit / 2)
        dut.io.tx.expect(expected.B)
        // Continue to next bit
        dut.clock.step(clocksPerBit / 2)
    }

    // Start with raw data
    val rawData: Int = 65 // ASCII 'A' = 0x41 = 0b01000001
    val paddedData = rawData & 0xFF // Ensure 8 bits
    
    println(f"Original data: ${paddedData.toChar} (0x${paddedData}%02X = 0b${paddedData.toBinaryString})")
    
    // Reverse by creating a new int bit by bit
    val reversedData = {
        var result = 0
        for (i <- 0 until 8) {
            if ((paddedData & (1 << i)) != 0) {
                result |= (1 << (7 - i))
            }
        }
        result
    }

    println(f"Reversed data: 0x${reversedData}%02X = 0b${reversedData.toBinaryString.padTo(8, '0')}")

    // Extract bits in order they'll appear
    val dataBits = for (i <- 0 until 8) yield {
        val bit = (reversedData >> i) & 1
        println(s"Bit $i: ${bit}")
        bit == 1
    }
    println(s"Expected wire bits: ${dataBits.mkString}")

    // Initiate transmission
    dut.io.txConfig.data.poke(rawData.U)
    dut.io.txConfig.load.poke(true.B)
    dut.clock.step(1)
    dut.io.txConfig.load.poke(false.B)

    // Wait for start bit (TX goes low)
    var timeout = 0
    while (dut.io.tx.peek().litToBoolean && timeout < clocksPerBit * 2) {
        dut.clock.step(1)
        timeout += 1
    }
    println("Found start bit!")

    // Check start bit
    waitAndCheckBit(false)

    // Check data bits
    for (i <- 0 to 7 by 1) {
        val bit = dataBits(i)
        println(s"Checking data bit $i: expected $bit")
        waitAndCheckBit(bit)
    }

    // Check stop bit
    println("Checking stop bit")
    waitAndCheckBit(true)

    // Final idle verification
    dut.clock.step(clocksPerBit / 2)
    dut.io.tx.expect(true.B)
}
}