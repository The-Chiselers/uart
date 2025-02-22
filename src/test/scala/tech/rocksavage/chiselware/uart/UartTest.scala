package tech.rocksavage.chiselware.uart

import chiseltest._
import chiseltest.simulator.{
    VerilatorBackendAnnotation,
    VerilatorCFlags,
    WriteFstAnnotation,
    WriteVcdAnnotation
}
import firrtl2.annotations.Annotation
import firrtl2.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tech.rocksavage.chiselware.uart.param.UartParams

class UartTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
    val numTests    = 2
    val testNameArg = System.getProperty("testName")
    // Command-line toggles
    val enableVcd = System.getProperty("enableVcd", "true").toBoolean
    val enableFst = System.getProperty("enableFst", "false").toBoolean
    val testName = (testNameArg == null || testNameArg == "") match {
        case true  => "regression"
        case false => testNameArg
    }

    println(s"Running test: $testName")
    val useVerilator = System.getProperty("useVerilator", "false").toBoolean
    val testDir      = "out/test"
    val backendAnnotations = {
        var annos: Seq[Annotation] = Seq()
        if (enableVcd) annos = annos :+ WriteVcdAnnotation
        if (enableFst) annos = annos :+ WriteFstAnnotation
        if (useVerilator) {
            annos = annos :+ VerilatorBackendAnnotation
            annos = annos :+ VerilatorCFlags(
              Seq("--std=c++17", "-O3", "-march=native")
            )
        }
        annos = annos :+ TargetDirAnnotation(testDir)
        annos
    }

    // Decide which test to run based on "testName"
    if (testName == "regression") {
        (1 to numTests).foreach { config =>
            runTest(s"UART_test_config_$config")
        }
    } else {
        // Single test
        runTest(testName)
    }

    def runTest(name: String): Unit = {
        behavior of name

        // Example UART parameters
        val uartParams = UartParams(
          dataWidth = 32,
          addressWidth = 32,
          maxClocksPerBit = 217,
          maxOutputBits = 8,
          syncDepth = 2
        )

        info(
          s"Data Width: ${uartParams.dataWidth}, Address Width: ${uartParams.addressWidth}"
        )
        info("--------------------------------")

        name match {

             // Add new FIFO test cases
            case "txFifoOverflow" =>
                it should "handle TX FIFO overflow correctly" in {
                    test(new UartTx(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            fifoIntegrationTests.txFifoOverflowTest(dut, uartParams)
                        }
                }

            case "rxFifoOverflow" =>
                it should "handle RX FIFO overflow correctly" in {
                    test(new UartRx(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            fifoIntegrationTests.rxFifoOverflowTest(dut, uartParams)
                        }
                }

            case "burstTransfer" =>
                it should "handle burst transfers correctly" in {
                    test(new FullDuplexUart(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            fifoIntegrationTests.burstTransferTest(dut, uartParams)
                        }
                }

            case "fifoStatus" =>
                it should "correctly report FIFO status" in {
                    test(new FullDuplexUart(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            fifoIntegrationTests.fifoStatusTest(dut, uartParams)
                        }
                }

            case "backToBackTransfer" =>
                it should "handle back-to-back transfers correctly" in {
                    test(new FullDuplexUart(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            fifoIntegrationTests.backToBackTransferTest(dut, uartParams)
                        }
                }

            case "fifoErrorHandling" =>
                it should "handle FIFO errors correctly" in {
                    test(new FullDuplexUart(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            fifoIntegrationTests.fifoErrorHandlingTest(dut, uartParams)
                        }
                }

            // Add Random Test Cases
            case "randomBaudRate" =>
                it should "handle random valid baud rates" in {
                    test(new Uart(uartParams, false))
                        .withAnnotations(backendAnnotations) { dut =>
                            randomTests.randomBaudRateTest(dut, uartParams)
                        }
                }

            case "randomDataPattern" =>
                it should "handle random data patterns" in {
                    test(new FullDuplexUart(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            randomTests.randomDataPatternTest(dut, uartParams)
                        }
                }

            case "randomNoise" =>
                it should "handle random noise patterns" in {
                    test(new FullDuplexUart(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            randomTests.randomNoiseTest(dut, uartParams)
                        }
                }

            case "randomParity" =>
                it should "handle random parity configurations" in {
                    test(new FullDuplexUart(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            randomTests.randomParityTest(dut, uartParams)
                        }
                }

            // Baud Rate Tests
            case "baudRateAccuracy" =>
                it should "maintain accurate baud rate timing" in {
                    test(new Uart(uartParams, false))
                        .withAnnotations(backendAnnotations) { dut =>
                            baudRateTests.baudRateAccuracyTest(dut, uartParams)
                        }
                }

            case "baudRateStability" =>
                it should "maintain stable baud rate over multiple transmissions" in {
                    test(new Uart(uartParams, false))
                        .withAnnotations(backendAnnotations) { dut =>
                            baudRateTests.baudRateStabilityTest(dut, uartParams)
                        }
                }

            // Parity Tests
            case "evenParityTransmit" =>
                it should "correctly transmit data with even parity" in {
                    test(new UartTx(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            parityTests.txOddParityTest(dut, uartParams)
                        }
                }

            case "oddParityReceive" =>
                it should "correctly receive data with odd parity" in {
                    test(new UartRx(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            parityTests.rxOddParityTest(dut, uartParams)
                        }
                }

            // Error Tests
            case "frameError" =>
                it should "detect frame errors correctly" in {
                    test(new Uart(uartParams, false))
                        .withAnnotations(backendAnnotations) { dut =>
                            errorTests.frameErrorTest(dut, uartParams)
                        }
                }

            case "startBitError" =>
                it should "detect start bit errors correctly" in {
                    test(new Uart(uartParams, false))
                        .withAnnotations(backendAnnotations) { dut =>
                            errorTests.startBitErrorTest(dut, uartParams)
                        }
                }

            case "parityError" =>
                it should "detect wrong parity errors correctly" in {
                    test(new Uart(uartParams, false))
                        .withAnnotations(backendAnnotations) { dut =>
                            errorTests.parityErrorTest(dut, uartParams)
                        }
                }

            // Basic Transmission Tests
            case "basicTransmit" =>
                it should "transmit data correctly" in {
                    test(new UartTx(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            transmissionTests.basicTxTest(dut, uartParams)
                        }
                }

            case "basicReceive" =>
                it should "receive data correctly" in {
                    test(new UartRx(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            transmissionTests.basicRxTest(dut, uartParams)
                        }
                }

            // FullDuplex Tests
            case "bidirectionalComm" =>
                it should "handle bidirectional communication" in {
                    test(new FullDuplexUart(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            fullDuplexTests.bidirectionalCommunicationTest(
                              dut,
                              uartParams
                            )
                        }
                }

            case "simultaneousTransmit" =>
                it should "handle simultaneous transmission" in {
                    test(new FullDuplexUart(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            fullDuplexTests.simultaneousTransmissionTest(
                              dut,
                              uartParams
                            )
                        }
                }

            case "mixedBaudRate" =>
                it should "handle mixed baud rates" in {
                    test(new FullDuplexUart(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            fullDuplexTests.mixedBaudRateTest(dut, uartParams)
                        }
                }

            case "highSpeedTransmit" =>
                it should "handle high-speed transmission" in {
                    test(new FullDuplexUart(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            fullDuplexTests.highSpeedTransmissionTest(
                              dut,
                              uartParams
                            )
                        }
                }

            case "longTransmission" =>
                it should "handle long transmissions" in {
                    test(new FullDuplexUart(uartParams)).withAnnotations(
                      backendAnnotations
                    ) { dut =>
                        fullDuplexTests.longTransmissionTest(dut, uartParams)
                    }
                }

            case "errorRecovery" =>
                it should "recover from errors" in {
                    test(new FullDuplexUart(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            fullDuplexTests.errorRecoveryTest(dut, uartParams)
                        }
                }

            case "noiseImmunity" =>
                it should "be immune to noise" in {
                    test(new FullDuplexUart(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            fullDuplexTests.noiseImmunityTest(dut, uartParams)
                        }
                }

            case "baudRateSwitch" =>
                it should "handle baud rate switching" in {
                    test(new FullDuplexUart(uartParams)).withAnnotations(
                      backendAnnotations
                    ) { dut =>
                        fullDuplexTests.baudRateSwitchingTest(dut, uartParams)
                    }
                }

            case "lineIdle" =>
                it should "detect line idle correctly" in {
                    test(new FullDuplexUart(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            fullDuplexTests.lineIdleTest(dut, uartParams)
                        }
                }

            case "fullDuplex" =>
                it should "handle bidirectional communication" in {
                    test(new FullDuplexUart(uartParams))
                        .withAnnotations(backendAnnotations) { dut =>
                            fullDuplexTests.bidirectionalCommunicationTest(
                              dut,
                              uartParams
                            )
                        }
                }

            // default => run all tests
            case _ =>
                runAllTests(uartParams)
        }
    }

    def runAllTests(params: UartParams): Unit = {
        baudRateTestsFull(params)
        parityTestsFull(params)
        errorTestsFull(params)
        transmissionTestsFull(params)
        fullDuplexTestsFull(params)
        randomTestsFull(params)
        fifoTestsFull(params)
    }

    def fifoTestsFull(params: UartParams): Unit = {
        it should "handle TX FIFO overflow correctly" in {
            test(new UartTx(params)).withAnnotations(backendAnnotations) { dut =>
                fifoIntegrationTests.txFifoOverflowTest(dut, params)
            }
        }

        it should "handle RX FIFO overflow correctly" in {
            test(new UartRx(params)).withAnnotations(backendAnnotations) { dut =>
                fifoIntegrationTests.rxFifoOverflowTest(dut, params)
            }
        }

        it should "handle burst transfers correctly" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    fifoIntegrationTests.burstTransferTest(dut, params)
                }
        }

        it should "correctly report FIFO status" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    fifoIntegrationTests.fifoStatusTest(dut, params)
                }
        }

        it should "handle back-to-back transfers correctly" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    fifoIntegrationTests.backToBackTransferTest(dut, params)
                }
        }

        it should "handle FIFO errors correctly" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    fifoIntegrationTests.fifoErrorHandlingTest(dut, params)
                }
        }
    }

    def randomTestsFull(params: UartParams): Unit = {
        it should "handle random valid baud rates" in {
            test(new Uart(params, false))
                .withAnnotations(backendAnnotations) { dut =>
                    randomTests.randomBaudRateTest(dut, params)
                }
        }

        it should "handle random data patterns" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    randomTests.randomDataPatternTest(dut, params)
                }
        }

        it should "handle random noise patterns" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    randomTests.randomNoiseTest(dut, params)
                }
        }

        it should "handle random parity configurations" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    randomTests.randomParityTest(dut, params)
                }
        }
    }

    def baudRateTestsFull(params: UartParams): Unit = {
        it should "maintain accurate baud rate timing" in {
            test(new Uart(params, false)).withAnnotations(backendAnnotations) {
                dut =>
                    baudRateTests.baudRateAccuracyTest(dut, params)
            }
        }

        it should "maintain stable baud rate over multiple transmissions" in {
            test(new Uart(params, false)).withAnnotations(backendAnnotations) {
                dut =>
                    baudRateTests.baudRateStabilityTest(dut, params)
            }
        }
    }

    def parityTestsFull(params: UartParams): Unit = {
        it should "correctly transmit data with even parity" in {
            test(new UartTx(params)).withAnnotations(backendAnnotations) {
                dut =>
                    parityTests.txOddParityTest(dut, params)
            }
        }

        it should "correctly receive data with odd parity" in {
            test(new UartRx(params)).withAnnotations(backendAnnotations) {
                dut =>
                    parityTests.rxOddParityTest(dut, params)
            }
        }
    }

    def transmissionTestsFull(params: UartParams): Unit = {
        it should "transmit data correctly" in {
            test(new UartTx(params)).withAnnotations(backendAnnotations) {
                dut =>
                    transmissionTests.basicTxTest(dut, params)
            }
        }

        it should "receive data correctly" in {
            test(new UartRx(params)).withAnnotations(backendAnnotations) {
                dut =>
                    transmissionTests.basicRxTest(dut, params)
            }
        }
    }

    def fullDuplexTestsFull(params: UartParams): Unit = {
        it should "handle bidirectional communication" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    fullDuplexTests.bidirectionalCommunicationTest(dut, params)
                }
        }

        it should "handle simultaneous transmission" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    fullDuplexTests.simultaneousTransmissionTest(dut, params)
                }
        }

        it should "handle mixed baud rates" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    fullDuplexTests.mixedBaudRateTest(dut, params)
                }
        }

        it should "handle high-speed transmission" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    fullDuplexTests.highSpeedTransmissionTest(dut, params)
                }
        }

        it should "handle long transmissions" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    fullDuplexTests.longTransmissionTest(dut, params)
                }
        }

        it should "recover from errors" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    fullDuplexTests.errorRecoveryTest(dut, params)
                }
        }

        it should "be immune to noise" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    fullDuplexTests.noiseImmunityTest(dut, params)
                }
        }

        it should "handle baud rate switching" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    fullDuplexTests.baudRateSwitchingTest(dut, params)
                }
        }

        it should "detect line idle correctly" in {
            test(new FullDuplexUart(params))
                .withAnnotations(backendAnnotations) { dut =>
                    fullDuplexTests.lineIdleTest(dut, params)
                }
        }
    }

    def errorTestsFull(params: UartParams): Unit = {
        it should "detect frame errors correctly" in {
            test(new Uart(params, false)).withAnnotations(backendAnnotations) {
                dut =>
                    errorTests.frameErrorTest(dut, params)
            }
        }

        it should "detect start bit errors correctly" in {
            test(new Uart(params, false)).withAnnotations(backendAnnotations) {
                dut =>
                    errorTests.startBitErrorTest(dut, params)
            }
        }

        it should "detect wrong parity errors correctly" in {
            test(new Uart(params, false)).withAnnotations(backendAnnotations) {
                dut =>
                    errorTests.parityErrorTest(dut, params)
            }
        }
    }

    // Coverage collection helper
    def coverageCollection(
        cov: Seq[Annotation],
        params: UartParams,
        testName: String
    ): Unit = {}
}
