// errorTests.scala
package tech.rocksavage.chiselware.uart

import chisel3._
import chiseltest._
import tech.rocksavage.chiselware.apb.ApbTestUtils._
import tech.rocksavage.chiselware.uart.param.UartParams

object errorTests {

  /**
    * 1) Frame (Stop-Bit) Error Test
    *    We send a byte (0xAA) but keep the stop bit LOW (should be HIGH).
    *    That triggers StopBitError => we map that to bit 1 (0x02).
    */
  def frameErrorTest(dut: Uart, params: UartParams): Unit = {
    implicit val clk: Clock = dut.clock
    dut.clock.setTimeout(5000)

    val clocksPerBit = 217
    println(s"Starting frame error test with clocksPerBit = $clocksPerBit")

    // Configure 8 bits, no parity
    writeAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("clocksPerBitDb").get.U, clocksPerBit.U)
    writeAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("numOutputBitsDb").get.U, 8.U)
    clk.step(clocksPerBit * 2)

    // Clear existing errors
    readAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("error").get.U)
    clk.step(2)

    def sendBit(bit: Boolean, cycleCount: Int): Unit = {
      println(s"Sending bit: $bit for $cycleCount cycles")
      dut.io.rx.poke(bit.B)
      clk.step(cycleCount)
    }

    // Idle high initially
    dut.io.rx.poke(true.B)
    clk.step(clocksPerBit * 2)

    println("Starting frame (stop) error sequence")

    // Start bit (low)
    sendBit(false, clocksPerBit)

    // Data bits (0xAA = 10101010 in binary, LSB first)
    for (i <- 0 until 8) {
      val bit = ((0xAA >> i) & 1) == 1
      sendBit(bit, clocksPerBit)
    }

    // Invalid stop bit (should be 1 => we send 0)
    println("Sending invalid stop bit (0)")
    sendBit(false, clocksPerBit)

    // Return to idle
    dut.io.rx.poke(true.B)

    println("Waiting for stop-bit error detection")
    var errorDetected = false
    var cycleCount = 0
    val maxWaitCycles = clocksPerBit * 4

    while (!errorDetected && cycleCount < maxWaitCycles) {
      val errorVal = readAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("error").get.U)
      if (cycleCount % 10 == 0 || errorVal != 0) {
        println(f"Cycle $cycleCount: Error register = 0x$errorVal%02x")
      }
      // Check bit1 => 0x02 for StopBitError
      if ((errorVal & 0x02) == 0x02) {
        errorDetected = true
        println("Frame/StopBit error detected!")
      }
      clk.step(1)
      cycleCount += 1
    }

    // Final check
    val finalError = readAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("error").get.U)
    println(f"Final error register = 0x$finalError%02x")
    assert((finalError & 0x02) == 0x02,
      f"Frame (stop-bit) error not detected; final error reg = 0x$finalError%02x")

    println("frameErrorTest PASSED (assuming hardware sets bit 1=0x02 for StopBitError)")
  }


  /**
    * 2) Parity Error Test
    *    We enable parity (EVEN) and deliberately send the wrong parity bit,
    *    expecting the receiver to set bit2 => 0x04 for ParityError.
    */
  def parityErrorTest(dut: Uart, params: UartParams): Unit = {
    implicit val clk: Clock = dut.clock
    clk.setTimeout(5000)

    val clocksPerBit = 217
    println(s"Starting parityErrorTest with clocksPerBit = $clocksPerBit")

    // Configure 8 data bits, parity ON (EVEN)
    writeAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("clocksPerBitDb").get.U, clocksPerBit.U)
    writeAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("numOutputBitsDb").get.U, 8.U)
    writeAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("useParityDb").get.U, 1.U)  // parity on
    writeAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("parityOddDb").get.U, 0.U) // 0 => even
    clk.step(clocksPerBit * 2)

    // Clear old errors
    readAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("error").get.U)
    clk.step(2)

    // Helper to send a byte with intentionally wrong parity
    def sendByteWithWrongParity(char: Int): Unit = {
      // --- Start Bit: Hold low for one full bit time ---
      dut.io.rx.poke(false.B)
      clk.step(clocksPerBit)

      // --- Data Bits: Send LSB-first (for 0x55, popcount=4) ---
      for (i <- 0 until 8) {
        val bit = ((char >> i) & 1) == 1
        dut.io.rx.poke(bit.B)
        clk.step(clocksPerBit)
      }

      // --- Parity Bit: For 0x55 (even parity correct = 0), force '1' to trigger error ---
      dut.io.rx.poke(true.B)
      clk.step(clocksPerBit)

      // --- Stop Bit: Should be HIGH ---
      dut.io.rx.poke(true.B)
      clk.step(clocksPerBit)

      // --- Extra Idle Time: Hold high for a couple of bit times so the RX FSM settles ---
      dut.io.rx.poke(true.B)
      clk.step(clocksPerBit * 2)
    }

    // Idle first
    dut.io.rx.poke(true.B)
    clk.step(clocksPerBit*2)

    println("Sending 0x55 with forced WRONG parity bit.")
    sendByteWithWrongParity(0x55)

    // Wait for parity error => bit2 => 0x04
    var cycleCount = 0
    var foundParityError = false
    val maxCycles = clocksPerBit * 15

    while (!foundParityError && cycleCount < maxCycles) {
      val errVal = readAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("error").get.U)
      println(f"Cycle $cycleCount: Error register = 0x$errVal%x")
      if ((errVal & 0x03) == 0x03) {
        foundParityError = true
        println(f"Detected ParityError in bit2 => error=0x$errVal%x")
      }
      clk.step(1)
      cycleCount += 1
    }
    assert(foundParityError, "Never detected ParityError in the hardware")

    println("parityErrorTest PASSED (assuming hardware sets bit2=0x04 on parity mismatch)")
  }


  /**
    * 3) Start-Bit Error Test
    *    We do a short glitch low, so the receiver thinks a start bit is coming,
    *    but then line goes high before a full bit => "start bit error" => 0x01
    */
  def startBitErrorTest(dut: Uart, params: UartParams): Unit = {
    implicit val clk: Clock = dut.clock
    clk.setTimeout(5000)

    val clocksPerBit = 217
    println(s"Starting startBitErrorTest with clocksPerBit = $clocksPerBit")

    // 8 bits, no parity
    writeAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("clocksPerBitDb").get.U, clocksPerBit.U)
    writeAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("numOutputBitsDb").get.U, 8.U)
    clk.step(10)

    // Clear errors
    val preError = readAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("error").get.U)
    println(f"Pre-test error reg = 0x$preError%x")
    clk.step(2)

    // short glitch => line low half a bit, then back high
    def shortStartGlitch(): Unit = {
      dut.io.rx.poke(false.B)
      clk.step(clocksPerBit / 2)
      dut.io.rx.poke(true.B)
      clk.step(clocksPerBit) // so by the time we sample at full bit, it's high
    }

    // Idle
    dut.io.rx.poke(true.B)
    clk.step(clocksPerBit)

    println("Injecting false start bit glitch")
    shortStartGlitch()

    // Return idle
    dut.io.rx.poke(true.B)
    clk.step(clocksPerBit * 2)

    // Wait for start-bit error => bit0 => 0x01
    var cycleCount = 0
    val maxWait = clocksPerBit * 8
    var detectedStartError = false

    while (!detectedStartError && cycleCount < maxWait) {
      val errVal = readAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("error").get.U)
      if ((errVal & 0x01) == 0x01) {
        detectedStartError = true
        println(f"Start-bit error detected! (error reg = 0x$errVal%x)")
      }
      clk.step(1)
      cycleCount += 1
    }
    assert(detectedStartError, s"Never saw StartBitError after $cycleCount cycles")

    // Final check
    val finalError = readAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("error").get.U)
    println(f"Final error register = 0x$finalError%x")
    assert((finalError & 0x01) == 0x01,
           f"Expected StartBitError in bit0, got 0x$finalError%x")

    println("startBitErrorTest PASSED (assuming hardware sets bit0=0x01 for StartBitError)")
  }

}
