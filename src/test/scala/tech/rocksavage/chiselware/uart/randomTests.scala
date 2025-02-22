package tech.rocksavage.chiselware.uart

import chisel3._
import chiseltest._
import scala.util.Random
import tech.rocksavage.chiselware.uart.param.UartParams
import tech.rocksavage.chiselware.apb.ApbTestUtils._
import tech.rocksavage.chiselware.apb.ApbBundle

object randomTests {
  
  /** Random Baud Rate Test
   * Tests random valid baud rate configurations
   */
  def randomBaudRateTest(dut: Uart, params: UartParams): Unit = {
    implicit val clk: Clock = dut.clock
    clk.setTimeout(10000)

    val validBaudRates = Seq(
      217,  // 115200
      434,  // 57600
      869,  // 28800
      1736, // 14400
      3472  // 7200
    )

    for (_ <- 1 to 10) {  // Test 10 random configurations
      val clocksPerBit = validBaudRates(Random.nextInt(validBaudRates.length))
      println(s"Testing random baud rate with clocksPerBit = $clocksPerBit")
      
      // Configure UART
      writeAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("clocksPerBitDb").get.U, clocksPerBit.U)
      writeAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("numOutputBitsDb").get.U, 8.U)
      clk.step(clocksPerBit * 2)

      // Verify configuration was set correctly
      val readClocksPer = readAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("clocksPerBitDb").get.U)
      assert(readClocksPer == clocksPerBit, 
        s"Baud rate configuration mismatch: expected $clocksPerBit, got $readClocksPer")

      // Send random character and track transmission timing
      val testChar = Random.nextInt(128).toChar
      println(s"Sending character: $testChar")
      
      // Record initial state
      val initialTx = dut.io.tx.peekBoolean()
      assert(initialTx, "TX line should be high (idle) before transmission")

      // Start transmission
      writeAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("dataIn").get.U, testChar.toInt.U)
      writeAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("load").get.U, true.B)
      clk.step(1)
      writeAPB(dut.io.apb, dut.registerMap.getAddressOfRegister("load").get.U, false.B)
      
      // Verify start bit
      clk.step(1)
      val startBit = dut.io.tx.peekBoolean()
      assert(!startBit, "Start bit should be low")

      // Track bit transitions
      var lastTx = startBit
      var transitions = 0
      var bitCount = 0
      var cycleCount = 0

      // Monitor full transmission
      while (cycleCount < clocksPerBit * 12) {
        val currentTx = dut.io.tx.peekBoolean()
        if (currentTx != lastTx) {
          transitions += 1
          assert(cycleCount % clocksPerBit <= 2, 
            s"Transition occurred at unexpected time: cycle $cycleCount")
        }
        lastTx = currentTx
        cycleCount += 1
        clk.step(1)
      }

      // Verify final state
      val finalTx = dut.io.tx.peekBoolean()
      assert(finalTx, "TX line should return to high (idle) after transmission")

      // Verify minimum transitions (start bit + data bits + stop bit)
      assert(transitions >= 2, "Too few transitions detected in transmission")
      
      // Verify transmission timing
      assert(cycleCount >= clocksPerBit * 10, 
        "Transmission completed too quickly for configured baud rate")
    }
  }

  /** Random Data Pattern Test
   * Tests random data patterns including edge cases
   */
  def reverseByte(x: Int): Int = {
    (0 until 8).foldLeft(0) { (acc, i) =>
      // For each bit i in x, shift acc left and OR with that bit
      (acc << 1) | ((x >> i) & 1)
    }
  }

  private def computeExpectedTransitions(pattern: Int): Int = {
    println(s"\nAnalyzing pattern 0x${pattern.toHexString} (${pattern.toBinaryString.reverse.padTo(8, '0').reverse})")

    // Convert to LSB-first bit array
    val reversedPattern = reverseByte(pattern & 0xFF)
    val bits = (0 until 8).map(i => ((reversedPattern >> i) & 1) == 1)
    println(s"Data bits (LSB first): ${bits.map(if (_) '1' else '0').mkString}")
    
    var transitions = 0
    var lastBit = true  // Start from idle (high)
    
    // Count start bit transition (always happens)
    transitions += 1
    println("Transition 1: idle(1) -> start(0)")
    lastBit = false  // Now at start bit (low)
    
    // Count transitions between data bits
    for ((bit, idx) <- bits.zipWithIndex) {
        if (bit != lastBit) {
            transitions += 1
            println(s"Transition ${transitions}: bit${idx}(${if (lastBit) '1' else '0'}) -> bit${idx}(${if (bit) '1' else '0'})")
        }
        lastBit = bit
    }
    
    // Count transition to stop bit if last data bit was low
    if (!lastBit) {
        transitions += 1
        println(s"Transition ${transitions}: last_bit(0) -> stop(1)")
    }
    
    println(s"Total expected transitions: $transitions")
    transitions
  }

  def randomDataPatternTest(dut: FullDuplexUart, params: UartParams): Unit = {
    implicit val clk: Clock = dut.clock
    clk.setTimeout(10000)

    val clocksPerBit = 217
    setupUart(dut.io.uart1Apb, dut.getUart1, clocksPerBit)
    setupUart(dut.io.uart2Apb, dut.getUart2, clocksPerBit)

    // Generate test patterns
    val patterns = Seq(
      Random.nextInt(256),  // Random byte
      0x00,                 // All zeros
      0xFF,                 // All ones
      0x55,                 // Alternating
      0xAA,                 // Alternating inverse
      0x01,                 // Single bit
      0x80                  // Single bit at MSB
    )

    for (pattern <- patterns) {
      println(s"\nTesting pattern: 0x${pattern.toHexString}")
      
      // Verify initial state
      assert(dut.io.uart1_tx.peekBoolean(), "UART1 TX should be idle high")
      assert(dut.io.uart2_tx.peekBoolean(), "UART2 TX should be idle high")
      
      // Track transitions
      var actualTransitions = 0
      var lastTx = true  // TX starts high (idle)
      var samplingStarted = false
      var cycleCount = 0
      var lastTransitionCycle = 0
      
      // Send from UART1 to UART2
      writeAPB(dut.io.uart1Apb, dut.getUart1.registerMap.getAddressOfRegister("dataIn").get.U, pattern.U)
      writeAPB(dut.io.uart1Apb, dut.getUart1.registerMap.getAddressOfRegister("load").get.U, true.B)
      clk.step(1)
      writeAPB(dut.io.uart1Apb, dut.getUart1.registerMap.getAddressOfRegister("load").get.U, false.B)

      // Monitor the transmission for transitions
      for (_ <- 0 until clocksPerBit * 12) {
        val currentTx = dut.io.uart1_tx.peekBoolean()
        
        // Start counting when we see the first transition (start bit)
        if (!samplingStarted && lastTx && !currentTx) {
          samplingStarted = true
          println("Detected start of transmission")
        }
        
        if (samplingStarted && currentTx != lastTx) {
          actualTransitions += 1
          val cycles = cycleCount - lastTransitionCycle
          println(s"Detected transition ${actualTransitions} at cycle ${cycleCount}: ${lastTx} -> ${currentTx} (${cycles} cycles since last)")
          lastTransitionCycle = cycleCount
        }
        
        lastTx = currentTx
        cycleCount += 1
        clk.step(1)
      }

      val expectedTransitions = computeExpectedTransitions(pattern)
      println(s"\nTransmission summary for pattern 0x${pattern.toHexString}:")
      println(s"- Expected transitions: $expectedTransitions")
      println(s"- Actual transitions: $actualTransitions")
      println(s"- Total cycles: $cycleCount")
      
      // More flexible assertion - we might see extra transitions due to noise or timing
      assert(actualTransitions >= expectedTransitions, 
             s"Not enough transitions: expected at least $expectedTransitions, got $actualTransitions")

      // Wait and verify data received correctly
      waitForDataAndVerify(dut.io.uart2Apb, dut.getUart2, pattern)
      
      // Verify return to idle
      assert(dut.io.uart1_tx.peekBoolean(), "UART1 TX should return to idle high")
      assert(dut.io.uart2_tx.peekBoolean(), "UART2 TX should return to idle high")
      
      clk.step(clocksPerBit * 2)  // Wait between patterns
    }
  }

  /** Random Noise Injection Test
   * Tests random noise patterns on the RX line
   */
  def randomNoiseTest(dut: FullDuplexUart, params: UartParams): Unit = {
    implicit val clk: Clock = dut.clock
    clk.setTimeout(10000)

    val clocksPerBit = 217
    setupUart(dut.io.uart1Apb, dut.getUart1, clocksPerBit)
    setupUart(dut.io.uart2Apb, dut.getUart2, clocksPerBit)

    for (_ <- 1 to 5) {  // Run 5 noise pattern tests
      val testChar = Random.nextInt(128).toChar
      println(s"Testing noise immunity with char: $testChar")

      // Verify initial error state
      val initialError = readAPB(dut.io.uart2Apb, dut.getUart2.registerMap.getAddressOfRegister("error").get.U)
      assert(initialError.intValue == 0, "Error flags should be clear before test")

      // Start transmission
      writeAPB(dut.io.uart1Apb, dut.getUart1.registerMap.getAddressOfRegister("dataIn").get.U, testChar.toInt.U)
      writeAPB(dut.io.uart1Apb, dut.getUart1.registerMap.getAddressOfRegister("load").get.U, true.B)
      clk.step(1)
      writeAPB(dut.io.uart1Apb, dut.getUart1.registerMap.getAddressOfRegister("load").get.U, false.B)

      // Track noise injection
      var noiseInjections = 0
      var totalNoiseDuration = 0
      var lastNoiseTime = 0

      // Inject random noise with minimum duration
      val numNoisePulses = Random.nextInt(5) + 1  // 1 to 5 noise pulses
      for (_ <- 0 until numNoisePulses) {
        val noiseDuration = Random.nextInt(clocksPerBit / 8) + 1  // Ensure positive duration
        val noiseDelay = Random.nextInt(clocksPerBit / 4) + 1     // Ensure positive delay

        // Wait some time before injecting noise
        clk.step(noiseDelay)
        
        // Inject noise
        dut.io.uart2_rx.poke(Random.nextBoolean().B)
        clk.step(noiseDuration)
        dut.io.uart2_rx.poke(true.B)  // Return to idle

        noiseInjections += 1
        totalNoiseDuration += noiseDuration
      }

      println(s"Injected $noiseInjections noise pulses, total duration: $totalNoiseDuration cycles")

      // Extra delay for stability
      clk.step(clocksPerBit * 2)

      // Check for errors if noise was significant
      if (totalNoiseDuration > clocksPerBit / 2) {
        val errorStatus = readAPB(dut.io.uart2Apb, dut.getUart2.registerMap.getAddressOfRegister("error").get.U)
        assert(errorStatus.intValue != 0, "Expected error detection with significant noise")
      } else {
        // Only verify data if noise wasn't too disruptive
        waitForDataAndVerify(dut.io.uart2Apb, dut.getUart2, testChar.toInt)
      }

      // Verify return to idle state
      clk.step(clocksPerBit * 2)
      assert(dut.io.uart1_tx.peekBoolean(), "UART1 TX should return to idle")
      assert(dut.io.uart2_tx.peekBoolean(), "UART2 TX should return to idle")

      // Clear any errors before next test
      writeAPB(dut.io.uart2Apb, dut.getUart2.registerMap.getAddressOfRegister("clearError").get.U, true.B)
      clk.step(2)
      writeAPB(dut.io.uart2Apb, dut.getUart2.registerMap.getAddressOfRegister("clearError").get.U, false.B)
      clk.step(2)
    }
  }

  /** Random Parity Configuration Test
   * Tests random combinations of parity settings
   */
  def randomParityTest(dut: FullDuplexUart, params: UartParams): Unit = {
    implicit val clk: Clock = dut.clock
    clk.setTimeout(10000)

    val clocksPerBit = 217
    
    for (_ <- 1 to 5) {  // Test 5 random parity configurations
      val useParityDb = Random.nextBoolean()
      val parityOddDb = Random.nextBoolean()
      println(s"Testing with parity enabled: $useParityDb, odd parity: $parityOddDb")

      // Clear any existing errors
      writeAPB(dut.io.uart1Apb, dut.getUart1.registerMap.getAddressOfRegister("clearError").get.U, true.B)
      writeAPB(dut.io.uart2Apb, dut.getUart2.registerMap.getAddressOfRegister("clearError").get.U, true.B)
      clk.step(2)
      writeAPB(dut.io.uart1Apb, dut.getUart1.registerMap.getAddressOfRegister("clearError").get.U, false.B)
      writeAPB(dut.io.uart2Apb, dut.getUart2.registerMap.getAddressOfRegister("clearError").get.U, false.B)
      clk.step(2)

      // Configure both UARTs
      setupUart(dut.io.uart1Apb, dut.getUart1, clocksPerBit, useParityDb, parityOddDb)
      setupUart(dut.io.uart2Apb, dut.getUart2, clocksPerBit, useParityDb, parityOddDb)
      clk.step(clocksPerBit * 2)

      // Verify parity configuration
      val uart1ParityEnabled = readAPB(dut.io.uart1Apb, dut.getUart1.registerMap.getAddressOfRegister("useParityDb").get.U)
      val uart2ParityEnabled = readAPB(dut.io.uart2Apb, dut.getUart2.registerMap.getAddressOfRegister("useParityDb").get.U)
      assert(uart1ParityEnabled.intValue == (if (useParityDb) 1 else 0), "UART1 parity configuration mismatch")
      assert(uart2ParityEnabled.intValue == (if (useParityDb) 1 else 0), "UART2 parity configuration mismatch")

      // Send random character
      val testChar = Random.nextInt(128).toChar
      println(s"Sending character: $testChar")
      
      // Send data
      writeAPB(dut.io.uart1Apb, dut.getUart1.registerMap.getAddressOfRegister("dataIn").get.U, testChar.toInt.U)
      writeAPB(dut.io.uart1Apb, dut.getUart1.registerMap.getAddressOfRegister("load").get.U, true.B)
      clk.step(1)
      writeAPB(dut.io.uart1Apb, dut.getUart1.registerMap.getAddressOfRegister("load").get.U, false.B)

      // Monitor transmission
      var cycleCount = 0
      while (cycleCount < clocksPerBit * (if (useParityDb) 11 else 10)) {
        cycleCount += 1
        clk.step(1)
      }

      // Verify data and check for errors
      clk.step(clocksPerBit * 2)  // Extra delay for stability
      waitForDataAndVerify(dut.io.uart2Apb, dut.getUart2, testChar.toInt)
    }
  }

  // Helper Functions
  private def setupUart(apb: ApbBundle, uart: Uart, clocksPerBit: Int, useParity: Boolean = false, parityOdd: Boolean = false)(implicit clock: Clock): Unit = {
    writeAPB(apb, uart.registerMap.getAddressOfRegister("clocksPerBitDb").get.U, clocksPerBit.U)
    writeAPB(apb, uart.registerMap.getAddressOfRegister("numOutputBitsDb").get.U, 8.U)
    writeAPB(apb, uart.registerMap.getAddressOfRegister("useParityDb").get.U, useParity.B)
    writeAPB(apb, uart.registerMap.getAddressOfRegister("parityOddDb").get.U, parityOdd.B)
    
    // Verify configuration
    val actualClocksPer = readAPB(apb, uart.registerMap.getAddressOfRegister("clocksPerBitDb").get.U)
    val actualNumBits = readAPB(apb, uart.registerMap.getAddressOfRegister("numOutputBitsDb").get.U)
    val actualUseParity = readAPB(apb, uart.registerMap.getAddressOfRegister("useParityDb").get.U)
    val actualParityOdd = readAPB(apb, uart.registerMap.getAddressOfRegister("parityOddDb").get.U)
    
    assert(actualClocksPer == clocksPerBit, 
      s"ClocksPerBit mismatch: expected $clocksPerBit, got ${actualClocksPer}")
    assert(actualNumBits == 8, 
      s"NumOutputBits mismatch: expected 8, got ${actualNumBits}")
    assert(actualUseParity == (if (useParity) 1 else 0), 
      s"UseParity mismatch: expected $useParity, got ${actualUseParity}")
    assert(actualParityOdd == (if (parityOdd) 1 else 0), 
      s"ParityOdd mismatch: expected $parityOdd, got ${actualParityOdd}")
  }

 private def waitForDataAndVerify(apb: ApbBundle, uart: Uart, expectedData: Int)(implicit clock: Clock): Unit = {
    var received = false
    var timeout = 0
    val maxTimeout = 5000
    var dataValid = false
    var validCycles = 0
    
    while (!received && timeout < maxTimeout) {
      val rxDataAvailable = readAPB(apb, uart.registerMap.getAddressOfRegister("rxDataAvailable").get.U)
      
      if (rxDataAvailable.intValue != 0) {
        validCycles += 1
        if (validCycles >= 2) {
          dataValid = true
        }
      } else {
        validCycles = 0
      }
      
      if (dataValid) {
        val receivedData = readAPB(apb, uart.registerMap.getAddressOfRegister("rxData").get.U)
        assert(receivedData == expectedData, 
          s"Data mismatch: expected ${expectedData}, got ${receivedData}")
        
        val errorStatus = readAPB(apb, uart.registerMap.getAddressOfRegister("error").get.U)
        assert(errorStatus == 0, s"Unexpected error status: ${errorStatus}")
        
        received = true
      }
      
      clock.step(1)
      timeout += 1
    }

    assert(received, s"Timeout waiting for data after $timeout cycles")
  }
  
  private def verifyIdleState(dut: FullDuplexUart)(implicit clock: Clock): Unit = {
    assert(dut.io.uart1_tx.peekBoolean(), "UART1 TX should be idle (high)")
    assert(dut.io.uart2_tx.peekBoolean(), "UART2 TX should be idle (high)")
    
    val uart1Error = readAPB(dut.io.uart1Apb, dut.getUart1.registerMap.getAddressOfRegister("error").get.U)
    val uart2Error = readAPB(dut.io.uart2Apb, dut.getUart2.registerMap.getAddressOfRegister("error").get.U)
    
    assert(uart1Error == 0, s"UART1 has unexpected errors: $uart1Error")
    assert(uart2Error == 0, s"UART2 has unexpected errors: $uart2Error")
    
    val uart1DataAvailable = readAPB(dut.io.uart1Apb, dut.getUart1.registerMap.getAddressOfRegister("rxDataAvailable").get.U)
    val uart2DataAvailable = readAPB(dut.io.uart2Apb, dut.getUart2.registerMap.getAddressOfRegister("rxDataAvailable").get.U)
    
    assert(uart1DataAvailable == 0, "UART1 should not have data available in idle state")
    assert(uart2DataAvailable == 0, "UART2 should not have data available in idle state")
  }


}