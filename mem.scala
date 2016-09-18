import Chisel._

class PseudoMem extends Module{
  // for a cache, memory, as backend, is needed.
  // This memory return load request with a block, in which content is always the correponding address
  val io = new Bundle{
    val read_req = Bool(INPUT)
    val write_req = Bool(INPUT)
    val addr = UInt(INPUT, 32)
    val wb_data = UInt(INPUT, 256)
    val ld_data = UInt(OUTPUT, 256)
    val write_resp = Bool(OUTPUT)
    val read_resp = Bool(OUTPUT)  // should be valid with data
  }
  val read_latency = Reg(init = UInt(8, 4))
  val write_latency = Reg(init = UInt(2, 4))

  val (st_ready :: st_writing :: st_reading :: Nil) = Enum(UInt(), 3)
  val state = Reg(init = st_ready)
  val counter = Reg(init = UInt(0, 4)) // after every request, coutner should be set to 0

  val resped = Reg(init = Bool(false))
  val has_new_req = Bool()
  has_new_req := state === st_ready && !resped && (io.read_req | io.write_req)
  // So, in cycle just after responsing, request must not be sent

  val buffer = Reg(init = UInt(0, 256))
  io.read_resp := resped & io.read_req
  io.write_resp := resped & io.write_req
  io.ld_data := buffer

  switch(state) {
    is(st_ready) {
      when (has_new_req) {
        assert(io.read_req | io.write_req)
        assert(!(io.read_req & io.write_req))
        when (io.read_req) {
          counter := read_latency
          state := st_reading
        }
        when (io.write_req) {
          counter := write_latency
          state := st_writing
        }
        resped := Bool(false)
      }
    }
    is (st_writing) {
      counter := counter - UInt(1, 4)
      when (counter === UInt(0, 4)) {
        resped := Bool(true)
        state := st_ready
        counter := UInt(0, 4)
      }
    }
    is (st_reading) {
      counter := counter - UInt(1, 4)
      when (counter === UInt(0, 4)) {
        for (i <- 0 until 8) {
          buffer(i*32 + 31, i*32) := io.addr + UInt(i*4, 32)
        }
        resped := Bool(true)
        state := st_ready
        counter := UInt(0, 4)
      }
    }
  }
}

class MemTest(c: PseudoMem) extends Tester(c) {
  poke(c.io.read_req, true)
  poke(c.io.write_req, false)
  poke(c.io.addr, 0x100)
  step(100)
}

object inst {
  def main(args: Array[String]): Unit = {
    chiselMainTest(args,
      () => Module(new PseudoMem())) { c => new MemTest(c)}
  }
}
