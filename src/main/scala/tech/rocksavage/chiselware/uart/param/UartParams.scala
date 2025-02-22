// (c) 2024 Rocksavage Technology, Inc.
// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)

package tech.rocksavage.chiselware.uart.param

/** Default parameter settings for the Timer
  *
  * @constructor
  *   default parameter settings
  * @param dataWidth
  *   specifies the width of the data bus
  * @param addressWidth
  *   specifies the width of the address bus
  * @param countWidth
  *   specifies the size of the counter
  * @author
  *   Warren Savage
  * @version 1.0
  *
  * @see
  *   [[http://www.rocksavage.tech]] for more information
  */

case class UartParams(
    // Parameters for addressing
    dataWidth: Int = 32,
    addressWidth: Int = 32,
    maxClocksPerBit: Int = 100000,
    maxOutputBits: Int = 8,
    syncDepth: Int = 2,
    fifoDepth: Int = 2, 
    parity: Boolean = false,
    verbose: Boolean = false
) {

    require(dataWidth >= 1, "Data Width must be greater than or equal 1")
    require(addressWidth >= 1, "Address Width must be greater than or equal 1")
    require(
      maxClocksPerBit >= 1,
      "Clocks per bit must be greater than or equal 1"
    )
    require(maxOutputBits >= 1, "Output bits must be greater than or equal 1")
    require(syncDepth >= 0, "Sync depth must be greater than or equal 0")
}
