// This code is licensed under the Apache Software License 2.0 (see LICENSE.MD)
package tech.rocksavage.chiselware.uart

import chisel3.{Bool, UInt}

object UartParity {

    def parityChisel(data: UInt, parityOdd: Bool): Bool = {
        data.xorR ^ parityOdd
    }

    def parity(data: Int, parityOdd: Boolean): Boolean = {
//        data.xorR ^ parityOdd
        xorReduce(data) ^ parityOdd
    }

    def xorReduce(data: Int): Boolean = {
        data.toBinaryString.count(_ == '1') % 2 == 1
    }
}
