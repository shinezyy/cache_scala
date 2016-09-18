/**
  * Created by zyy on 16-9-18.
  */

import Chisel._

class ValidMem extends Module (addr_width: Int, data_width: Int) {
  val io = new Bundle{
    val wren = Bool(INPUT)
    val data_in = UInt(INPUT, data_width)
    val addr = UInt(INPUT, addr_width)
    val data_out = UInt(OUTPUT, data_width)
  }

  val depth = 2**addr_width
  val v_mem = Mem(depth, UInt(width=datawidth))
  when (reset) {
    for (i <- 0 until depth) {
      v_mem.write (i, UInt(0, data_width))
    }
  }
  when (!reset && io.wren) {
    v_mem.write(io.addr, io.data_in)
  }
  io.data_out := v_mem(io.addr)
}