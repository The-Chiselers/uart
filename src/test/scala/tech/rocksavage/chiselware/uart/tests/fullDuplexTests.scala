package tech.rocksavage.chiselware.uart.tests

import chisel3._
import chiseltest._
import tech.rocksavage.chiselware.apb.ApbBundle
import tech.rocksavage.chiselware.apb.ApbTestUtils._
import tech.rocksavage.chiselware.uart.hw.Uart
import tech.rocksavage.chiselware.uart.testconfig.UartTestConfig
import tech.rocksavage.chiselware.uart.testmodules.FullDuplexUart
import tech.rocksavage.chiselware.uart.testutils.rx.UartRxSetupTestUtils.receiveSetup
import tech.rocksavage.chiselware.uart.testutils.top.UartTopSetupTestUtils.setupUart
import tech.rocksavage.chiselware.uart.testutils.tx.UartTxSetupTestUtils.transmitSetup
import tech.rocksavage.chiselware.uart.types.param.UartParams

object fullDuplexTests {

    /** Basic bidirectional communication test
      */
    def bidirectionalCommunicationTest(
        dut: FullDuplexUart,
        params: UartParams
    ): Unit = {
        implicit val clock: Clock = dut.clock
        clock.setTimeout(0)

        val clockFrequency = 25_000_000
        val baudRate       = 2_500_000

        val clocksPerBit  = clockFrequency / (baudRate / 2)
        val numOutputBits = 8

        // Configure both UARTs

        val config: UartTestConfig = UartTestConfig(
          baudRate = baudRate,
          clockFrequency = clockFrequency,
          numOutputBits = numOutputBits
        )

        setupUart(dut.getUart1.registerMap, dut.io.uart1Apb, config)
        setupUart(dut.getUart2.registerMap, dut.io.uart2Apb, config)

        val dataFromUart1 = "Hello"
        val dataFromUart2 = "World"

        for (
          i <- 0 until math.min(dataFromUart1.length, dataFromUart2.length)
        ) {
            // Send from UART1 and wait for UART2 to receive
            sendChar(
              dut.io.uart1Apb,
              dut.getUart1,
              dataFromUart1(i),
              clocksPerBit
            )
            pollForRxData(dut.io.uart2Apb, dut.getUart2) match {
                case Some(uart2Received) =>
                    assert(
                      uart2Received == dataFromUart1(i),
                      f"UART2 received wrong data: got ${uart2Received}%c, expected ${dataFromUart1(i)}%c"
                    )
                case None =>
                    assert(false, "UART2 did not receive data in time")
            }

            // Send from UART2 and wait for UART1 to receive
            sendChar(
              dut.io.uart2Apb,
              dut.getUart2,
              dataFromUart2(i),
              clocksPerBit
            )
            pollForRxData(dut.io.uart1Apb, dut.getUart1) match {
                case Some(uart1Received) =>
                    println(s"UART1 received data: ${uart1Received.toInt}")
                    assert(
                      uart1Received == dataFromUart2(i),
                      f"UART1 received wrong data: got ${uart1Received}%c, expected ${dataFromUart2(i)}%c"
                    )
                case None =>
                    assert(false, "UART1 did not receive data in time")
            }
        }
    }

    def pollForRxData(apb: ApbBundle, uart: Uart, timeoutCycles: Int = 1000)(
        implicit clock: Clock
    ): Option[Char] = {
        val rxDataAvailableAddr =
            uart.registerMap.getAddressOfRegister("rx_dataAvailable").get.U
        val rxDataAddr = uart.registerMap.getAddressOfRegister("rx_data").get.U

        var cycles        = 0
        var dataAvailable = false

        while (!dataAvailable && cycles < timeoutCycles) {
            val availableRaw = readAPB(apb, rxDataAvailableAddr)
            dataAvailable = (availableRaw != 0)

            if (cycles % 10 == 0) { // Print every 10 cycles to avoid log spam
                println(
                  s"pollForRxData: Cycle $cycles, availableRaw=$availableRaw, dataAvailable=$dataAvailable"
                )
            }

            clock.step(1)
            cycles += 1
        }

        if (dataAvailable) {
            val data = readAPB(apb, rxDataAddr)
            println(
              s"pollForRxData: Got data = ${data.toInt} (char='${data.toInt.toChar}')"
            )
            Some(data.toInt.toChar)
        } else {
            println("pollForRxData: Timeout waiting for data")
            None
        }
    }

    /** Tests simultaneous transmission from both UARTs
      */
    def simultaneousTransmissionTest(
        dut: FullDuplexUart,
        params: UartParams
    ): Unit = {
        implicit val clock: Clock = dut.clock
        clock.setTimeout(5000)

        val clockFrequency = 25_000_000
        val baudRate       = 2_500_000

        val clocksPerBit  = clockFrequency / (baudRate / 2)
        val numOutputBits = 8

        val config = UartTestConfig(
          baudRate = baudRate,
          clockFrequency = clockFrequency,
          numOutputBits = numOutputBits
        )

        setupUart(dut.getUart1.registerMap, dut.io.uart1Apb, config)
        setupUart(dut.getUart2.registerMap, dut.io.uart2Apb, config)

        println(
          "Starting simultaneous transmission test with clocksPerBit = " + clocksPerBit
        )

        // Configure both UARTs identically, no parity, 8 bits, etc.

        clock.step(clocksPerBit * 2) // let config settle

        // Clear any pending data
        readAPB(
          dut.io.uart1Apb,
          dut.getUart1.registerMap.getAddressOfRegister("rx_data").get.U
        )
        readAPB(
          dut.io.uart2Apb,
          dut.getUart2.registerMap.getAddressOfRegister("rx_data").get.U
        )

        // We want to send 'A' from UART1, and 'B' from UART2
        val char1 = 'A'
        val char2 = 'B'
        println(
          "Simultaneous transmit: UART1 -> " + char1 + ", UART2 -> " + char2
        )

        // Start BOTH transmissions at same time
        println("Starting BOTH transmissions now...")

        writeAPB(
          dut.io.uart1Apb,
          dut.getUart1.registerMap.getAddressOfRegister("tx_dataIn").get.U,
          char1.toInt.U
        )
        writeAPB(
          dut.io.uart2Apb,
          dut.getUart2.registerMap.getAddressOfRegister("tx_dataIn").get.U,
          char2.toInt.U
        )
        writeAPB(
          dut.io.uart1Apb,
          dut.getUart1.registerMap.getAddressOfRegister("tx_load").get.U,
          1.U
        )
        writeAPB(
          dut.io.uart2Apb,
          dut.getUart2.registerMap.getAddressOfRegister("tx_load").get.U,
          1.U
        )
        clock.step(1)

        // Clear loads
        writeAPB(
          dut.io.uart2Apb,
          dut.getUart2.registerMap.getAddressOfRegister("tx_load").get.U,
          0.U
        )
        writeAPB(
          dut.io.uart1Apb,
          dut.getUart1.registerMap.getAddressOfRegister("tx_load").get.U,
          0.U
        )

        // We'll track what data each side eventually gets
        var uart1Data     = 0
        var uart2Data     = 0
        var uart1Received = false
        var uart2Received = false

        // We'll run up to a maximum # of cycles, checking each time if data arrived
        val maxCycles  = clocksPerBit * 15
        var cycleCount = 0

        while ((!(uart1Received && uart2Received)) && cycleCount < maxCycles) {
            // Print TX lines every 10 cycles for debug
            if (cycleCount % 10 == 0) {
                val tx1 = dut.io.uart1_tx.peekBoolean()
                val tx2 = dut.io.uart2_tx.peekBoolean()
                println("Cycle " + cycleCount + ": TX1=" + tx1 + ", TX2=" + tx2)
            }

            // Check if UART2 has new data
            if (!uart2Received) {
                checkRxAvailableAndRead(dut.io.uart2Apb, dut.getUart2).foreach {
                    d =>
                        uart2Data = d
                        uart2Received = true
                        println("UART2 just received data: " + d.toChar)
                }
            }

            // Check if UART1 has new data
            if (!uart1Received) {
                checkRxAvailableAndRead(dut.io.uart1Apb, dut.getUart1).foreach {
                    d =>
                        uart1Data = d
                        uart1Received = true
                        println("UART1 just received data: " + d.toChar)
                }
            }

            clock.step(1)
            cycleCount += 1
        }

        println("Test completed after " + cycleCount + " cycles")
        println(
          "UART1 final data: " + (if (uart1Data > 0) uart1Data.toChar.toString
                                  else "None")
        )
        println(
          "UART2 final data: " + (if (uart2Data > 0) uart2Data.toChar.toString
                                  else "None")
        )

        // Final Asserts
        assert(
          uart1Received && uart2Received,
          "One or both UARTs never received data after " + cycleCount + " cycles"
        )
        assert(
          uart1Data.toChar == char2,
          "UART1 received wrong data: got " + uart1Data.toChar + ", expected " + char2
        )
        assert(
          uart2Data.toChar == char1,
          "UART2 received wrong data: got " + uart2Data.toChar + ", expected " + char1
        )
    }

    /** Helper method to check if rxDataAvailable is set, and if so, read
      * rxData. Returns Some(data) if new data arrived, else None.
      */
    def checkRxAvailableAndRead(apb: ApbBundle, uart: Uart)(implicit
        clock: Clock
    ): Option[Int] = {
        val available = readAPB(
          apb,
          uart.registerMap.getAddressOfRegister("rx_dataAvailable").get.U
        )
        if (available > 0) {
            println(
              "checkRxAvailableAndRead: rxDataAvailable=1, reading data..."
            )
            // Actually read the data
            val data = readAPB(
              apb,
              uart.registerMap.getAddressOfRegister("rx_data").get.U
            ).toInt
            println(
              "checkRxAvailableAndRead: got data=" + data + " (char=" + data.toChar + ")"
            )
            Some(data)
        } else {
            None
        }
    }

    /** Tests communication with different baud rates
      */
    def mixedBaudRateTest(dut: FullDuplexUart, params: UartParams): Unit = {
        implicit val clock: Clock = dut.clock
        clock.setTimeout(10000) // Extended timeout for slower baud rates

        val clockFrequency = 25_000_000
        val baudRate1      = 115_200
        val baudRate2      = 57_600

        val clocksPerBit1 = clockFrequency / (baudRate1 / 2)
        val clocksPerBit2 = clockFrequency / (baudRate2 / 2)
        val numOutputBits = 8

        println(s"Starting mixed baud rate test")
        println(s"UART1: clocksPerBit = $clocksPerBit1 (115200 baud)")
        println(s"UART2: clocksPerBit = $clocksPerBit2 (57600 baud)")

        val config1: UartTestConfig = UartTestConfig(
          baudRate = baudRate1,
          clockFrequency = clockFrequency,
          numOutputBits = numOutputBits
        )

        val config2: UartTestConfig = UartTestConfig(
          baudRate = baudRate2,
          clockFrequency = clockFrequency,
          numOutputBits = numOutputBits
        )

        // Configure UARTs and let settings settle
        receiveSetup(
          dut.getUart1.registerMap,
          dut.io.uart1Apb,
          config2
        )
        transmitSetup(
          dut.getUart1.registerMap,
          dut.io.uart1Apb,
          config1
        )
        receiveSetup(
          dut.getUart2.registerMap,
          dut.io.uart2Apb,
          config1
        )
        transmitSetup(
          dut.getUart2.registerMap,
          dut.io.uart2Apb,
          config2
        )
        val maxClocksPerBit = math.max(clocksPerBit1, clocksPerBit2)
        clock.step(maxClocksPerBit * 2) // Wait for slower UART to settle

        // Clear any pending data
        readAPB(
          dut.io.uart1Apb,
          dut.getUart1.registerMap.getAddressOfRegister("rx_data").get.U
        )
        readAPB(
          dut.io.uart2Apb,
          dut.getUart2.registerMap.getAddressOfRegister("rx_data").get.U
        )

        val testData = "Tes"
        for (char <- testData) {
            println(s"\nTesting character: '$char'")

            // Fast -> Slow (UART1 -> UART2)
            println(s"Sending from UART1 (fast) to UART2 (slow)")
            sendChar(dut.io.uart1Apb, dut.getUart1, char, clocksPerBit1)
            pollForRxData(dut.io.uart2Apb, dut.getUart2) match {
                case Some(uart2Received) =>
                    assert(
                      uart2Received != char,
                      f"UART2 unexpectedly received correct data despite baud rate mismatch"
                    )
                case None =>
                    assert(false, "UART2 did not receive data in time")
            }

            clock.step(clocksPerBit2 * 2) // Extra delay between transmissions

            // Slow -> Fast (UART2 -> UART1)
            println(s"Sending from UART2 (slow) to UART1 (fast)")
            sendChar(dut.io.uart2Apb, dut.getUart2, char, clocksPerBit2)
            pollForRxData(dut.io.uart1Apb, dut.getUart1) match {
                case Some(uart1Received) =>
                    println(s"UART1 received data: ${uart1Received.toInt}")
                    assert(
                      uart1Received != char,
                      f"UART1 unexpectedly received correct data despite baud rate mismatch"
                    )
                case None =>
                    assert(false, "UART1 did not receive data in time")
            }

            clock.step(clocksPerBit2 * 2) // Extra delay between characters
        }
    }

    /** Tests high-speed back-to-back transmission
      */
    def highSpeedTransmissionTest(
        dut: FullDuplexUart,
        params: UartParams
    ): Unit = {
        implicit val clock: Clock = dut.clock
        clock.setTimeout(0)

        val clockFrequency = 25_000_000
        val baudRate       = 921_600

        val clocksPerBit  = clockFrequency / (baudRate / 2)
        val numOutputBits = 8

        val config = UartTestConfig(
          baudRate = baudRate,
          clockFrequency = clockFrequency,
          numOutputBits = numOutputBits
        )

        setupUart(dut.getUart1.registerMap, dut.io.uart1Apb, config)
        setupUart(dut.getUart2.registerMap, dut.io.uart2Apb, config)

        val testData     = "HighSpeedTest"
        var receivedData = new StringBuilder

        // Send data with minimal inter-character delay
        for (char <- testData) {
            writeAPB(
              dut.io.uart1Apb,
              dut.getUart1.registerMap.getAddressOfRegister("tx_dataIn").get.U,
              char.toInt.U
            )
            writeAPB(
              dut.io.uart1Apb,
              dut.getUart1.registerMap.getAddressOfRegister("tx_load").get.U,
              true.B
            )
            clock.step(1)
            writeAPB(
              dut.io.uart1Apb,
              dut.getUart1.registerMap.getAddressOfRegister("tx_load").get.U,
              false.B
            )
            clock.step(clocksPerBit * 12)

            // Read received character
            receivedData.append(readChar(dut.io.uart2Apb, dut.getUart2))
        }

        assert(
          receivedData.toString == testData,
          s"Data mismatch at high speed: got ${receivedData.toString}, expected $testData"
        )
    }

    /** Tests long continuous transmission
      */
    def longTransmissionTest(dut: FullDuplexUart, params: UartParams): Unit = {
        implicit val clock: Clock = dut.clock
        clock.setTimeout(0)

        val clockFrequency = 1_000_000
        val baudRate       = 100_000

        val clocksPerBit  = clockFrequency / (baudRate / 2)
        val numOutputBits = 8

        val config = UartTestConfig(
          baudRate = baudRate,
          clockFrequency = clockFrequency,
          numOutputBits = numOutputBits
        )

        setupUart(dut.getUart1.registerMap, dut.io.uart1Apb, config)
        setupUart(dut.getUart2.registerMap, dut.io.uart2Apb, config)

        // Generate long test data
        val testData     = "Long transmission test with multiple sentences. "
        var receivedData = new StringBuilder

        for (char <- testData) {
            println(s"Sending character: $char")
            sendChar(dut.io.uart1Apb, dut.getUart1, char, clocksPerBit)
            val received = readChar(dut.io.uart2Apb, dut.getUart2)
            receivedData.append(received)
            println(s"Received character: $received")
            println(s"Received data so far: ${receivedData.toString}")
        }

        clock.setTimeout(10000)
        clock.step(
          2 * clocksPerBit
        ) // Extra delay to ensure all data is received

        assert(
          receivedData.toString == testData,
          "Data mismatch in long transmission\n" +
              s"Received: ${receivedData.toString}\n" +
              s"Expected: $testData"
        )
    }

    def readChar(apb: ApbBundle, uart: Uart)(implicit clock: Clock): Char = {
        val dataRegAddr = uart.registerMap.getAddressOfRegister("rx_data").get.U
        val data        = readAPB(apb, dataRegAddr)
        data.toChar
    }

    def sendChar(apb: ApbBundle, uart: Uart, char: Char, clocksPerBit: Int)(
        implicit clock: Clock
    ): Unit = {
        clock.setTimeout(100000)
        writeAPB(
          apb,
          uart.registerMap.getAddressOfRegister("tx_dataIn").get.U,
          char.toInt.U
        )
        writeAPB(
          apb,
          uart.registerMap.getAddressOfRegister("tx_load").get.U,
          true.B
        )
        clock.step(1)
        writeAPB(
          apb,
          uart.registerMap.getAddressOfRegister("tx_load").get.U,
          false.B
        )
        clock.step(
          clocksPerBit * 12
        ) // Wait for complete transmission including stop bit
    }

    /** Tests variable baud rate switching
      */
    def baudRateSwitchingTest(dut: FullDuplexUart, params: UartParams): Unit = {
        implicit val clock: Clock = dut.clock
        clock.setTimeout(0)

        val clockFrequency = 25_000_000

        val baudRates = Seq(
          115200,
          57600,
          28800
        )

        for (baudRate <- baudRates) {

            val config = UartTestConfig(
              baudRate = baudRate,
              clockFrequency = clockFrequency,
              numOutputBits = 8
            )

            setupUart(dut.getUart1.registerMap, dut.io.uart1Apb, config)
            setupUart(dut.getUart2.registerMap, dut.io.uart2Apb, config)

            val clocksPerBit = clockFrequency / (baudRate / 2)

            val testChar = 'R'
            sendChar(dut.io.uart1Apb, dut.getUart1, testChar, clocksPerBit)
            val received = readChar(dut.io.uart2Apb, dut.getUart2)

            assert(
              received == testChar,
              s"Character mismatch at baud rate $baudRate: got $received, expected $testChar"
            )

            clock.step(clocksPerBit * 2) // Wait between baud rate changes
        }
    }

    /** Tests line idle detection
      */
    def lineIdleTest(dut: FullDuplexUart, params: UartParams): Unit = {
        implicit val clock: Clock = dut.clock
        clock.setTimeout(0)

        val clockFrequency = 25_000_000
        val baudRate       = 115_200

        val clocksPerBit  = clockFrequency / (baudRate / 2)
        val numOutputBits = 8

        val config = UartTestConfig(
          baudRate = baudRate,
          clockFrequency = clockFrequency,
          numOutputBits = numOutputBits
        )

        setupUart(dut.getUart1.registerMap, dut.io.uart1Apb, config)
        setupUart(dut.getUart2.registerMap, dut.io.uart2Apb, config)

        // Verify initial idle state
        dut.io.uart1_tx.expect(true.B)
        dut.io.uart2_tx.expect(true.B)

        // Send a character and verify return to idle
        val testChar = 'I'
        sendChar(dut.io.uart1Apb, dut.getUart1, testChar, clocksPerBit)
        clock.step(clocksPerBit * 2)

        // Verify lines return to idle
        dut.io.uart1_tx.expect(true.B)
        dut.io.uart2_tx.expect(true.B)
    }
}
