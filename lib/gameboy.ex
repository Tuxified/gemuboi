defmodule Gameboy do
  alias Gameboy.{CPU, Memory}
  import Gameboy.Utils
  alias Gameboy.Instructions.{Arithmetic, Load}

  use Bitwise

  require IEx
  require Logger

  defstruct cpu: struct(CPU, []),
            memory: struct(Memory, []),
            running: false,
            breakpoint: 0xFFFF,
            log: false,
            cycles: 0,
            rom: 0

  def init do
    data = File.read!("DMG_ROM.gb")
    <<boot::binary-size(0x100), game::binary>> = File.read!("roms/Tetris.gb")
    gb = struct(Gameboy, running: true, breakpoint: 0x2A6)

    memory =
      gb.memory
      |> Memory.copy(data, 0)
      |> Memory.copy(game, 0x100)

    %{gb | memory: memory, rom: boot}
  end

  def run(%Gameboy{cpu: %{pc: %{value: breakpoint}}, breakpoint: breakpoint} = gb) do
    if IEx.started?() do
      IEx.pry()
    else
      %{gb | running: false}
    end
  end

  def run(%Gameboy{running: true, cycles: cycles} = gb) when cycles <= 70224 do
    tick(gb)
    |> run()
  end

  def run(%Gameboy{} = gb) do
    IO.puts("VBLANK!")

    tick(%{gb | cycles: gb.cycles - 70224})
    |> run()
  end

  def tick(%Gameboy{} = gb) do
    next = Memory.read8(gb.memory, gb.cpu.pc)
    opcode = Gameboy.Disassembler.lookup(next)
    cycles = if is_tuple(opcode.cycles), do: elem(opcode.cycles, 0), else: opcode.cycles

    if gb.log do
      # IO.puts(format("NEXT: 0x~2.16.0b; #{opcode}", [next]))
      # IO.inspect(gb.cpu)
      IO.puts(
        format(
          "[0x~2.16.0b] - AF: 0x~4.16.0b  BC: 0x~4.16.0b  DE: 0x~4.16.0b  HL: 0x~4.16.0b  PC: 0x~4.16.0b  SP: 0x~4.16.0b",
          [
            next,
            gb.cpu.af.value,
            gb.cpu.bc.value,
            gb.cpu.de.value,
            gb.cpu.hl.value,
            gb.cpu.pc.value,
            gb.cpu.sp.value
          ]
        )
      )
    end

    exec(%{gb | cycles: gb.cycles + cycles}, next)
  end

  def save_state(gb, file \\ "state.gb")

  def save_state(%Gameboy{} = gb, file) do
    File.write!(file, :erlang.term_to_binary(gb), [:write])
  end

  def load_state(file \\ "state.gb")

  def load_state(file) do
    file
    |> File.read!()
    |> :erlang.binary_to_term()
  end

  def screenshot(gb, file \\ "screenshot.png")

  def screenshot(gb, file) do
  end

  # stop once we're done with boot rom
  def exec(%Gameboy{cpu: %CPU{pc: %CPU.Register{value: pc}}, rom: rom} = gb, 0x00)
      when pc >= 0x100 and is_binary(rom) do
    memory = Memory.copy(gb.memory, gb.rom, 0)
    # IO.puts("BOOT ROM done at #{pc}, results are: ")
    # IO.inspect(gb.cpu)
    # , log: true}
    %{gb | memory: memory, rom: 0}
  end

  # NOP / NOOP
  def exec(%Gameboy{} = gb, 0x00), do: %{gb | cpu: CPU.inc_pc(gb.cpu)}

  # LD BC,d16
  def exec(%Gameboy{} = gb, 0x01) do
    val = Memory.read16(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)

    cpu =
      gb.cpu
      |> CPU.inc_pc(3)
      |> CPU.write_register(:bc, val)

    %{gb | cpu: cpu}
  end

  # LD (BC),A
  def exec(%Gameboy{} = gb, 0x02) do
    addr = CPU.read_register(gb.cpu, :bc)
    memory = Memory.write8(gb.memory, addr, CPU.read_register(gb.cpu, :a))

    %{gb | memory: memory, cpu: CPU.inc_pc(gb.cpu)}
  end

  # INC BC
  def exec(%Gameboy{} = gb, 0x03) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.inc_register(:bc)

    %{gb | cpu: cpu}
  end

  # INC B TODO check flags
  def exec(%Gameboy{} = gb, 0x04) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.inc_register(:b)

    %{gb | cpu: cpu}
  end

  # DEC B
  # Z 1 H -
  def exec(%Gameboy{} = gb, 0x05) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.dec_register(:b)

    %{gb | cpu: cpu}
  end

  # LD B,d8
  def exec(%Gameboy{} = gb, 0x06) do
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)

    cpu =
      gb.cpu
      |> CPU.inc_pc(2)
      |> CPU.write_register(:b, val)

    %{gb | cpu: cpu}
  end

  # LD (a16),SP
  def exec(%Gameboy{} = gb, 0x08) do
    addr = Memory.read16(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
    mem = Memory.write16(gb.memory, addr, CPU.read_register(gb.cpu, :sp))

    %{gb | cpu: CPU.inc_pc(gb.cpu, 3), memory: mem}
  end

  # ADD HL, BC
  # - 0 H C
  def exec(%Gameboy{} = gb, 0x09) do
    current_val = CPU.read_register(gb.cpu, :hl)
    val = CPU.read_register(gb.cpu, :bc)
    [result, <<flags::integer>>] = Arithmetic.add16(current_val, val)

    z = CPU.read_flag(gb.cpu, :z)
    f = <<z::size(1), flags <<< 1::size(7)>>
    <<flags::integer>> = f

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:hl, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # LD A,(BC)
  def exec(%Gameboy{} = gb, 0x0A) do
    addr = CPU.read_register(gb.cpu, :bc)
    val = Memory.read8(gb.memory, addr)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, val)

    %{gb | cpu: cpu}
  end

  # DEC BC
  def exec(%Gameboy{} = gb, 0x0B) do
    cpu =
      gb.cpu
      |> CPU.dec_register(:bc)
      |> CPU.inc_pc()

    %{gb | cpu: cpu}
  end

  # INC C
  def exec(%Gameboy{} = gb, 0x0C) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.inc_register(:c)

    %{gb | cpu: cpu}
  end

  # DEC C
  # Z 1 H -
  def exec(%Gameboy{} = gb, 0x0D) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.dec_register(:c)

    %{gb | cpu: cpu}
  end

  # LD C,d8
  def exec(%Gameboy{} = gb, 0x0E) do
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)

    cpu =
      gb.cpu
      |> CPU.inc_pc(2)
      |> CPU.write_register(:c, val)

    %{gb | cpu: cpu}
  end

  # LD DE,d16
  def exec(%Gameboy{} = gb, 0x11) do
    val = Memory.read16(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)

    cpu =
      gb.cpu
      |> CPU.inc_pc(3)
      |> CPU.write_register(:de, val)

    %{gb | cpu: cpu}
  end

  # LD (DE),A
  def exec(%Gameboy{} = gb, 0x12) do
    addr = CPU.read_register(gb.cpu, :de)
    memory = Memory.write8(gb.memory, addr, CPU.read_register(gb.cpu, :a))

    %{gb | memory: memory, cpu: CPU.inc_pc(gb.cpu)}
  end

  # INC DE
  def exec(%Gameboy{} = gb, 0x13) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.inc_register(:de)

    %{gb | cpu: cpu}
  end

  # INC D
  # Z 0 H -
  def exec(%Gameboy{} = gb, 0x14) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.inc_register(:d)

    %{gb | cpu: cpu}
  end

  # DEC D
  # Z 1 H -
  def exec(%Gameboy{} = gb, 0x15) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.dec_register(:d)

    %{gb | cpu: cpu}
  end

  # LD D,d8
  def exec(%Gameboy{} = gb, 0x16) do
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)

    cpu =
      gb.cpu
      |> CPU.inc_pc(2)
      |> CPU.write_register(:d, val)

    %{gb | cpu: cpu}
  end

  # RLA
  def exec(%Gameboy{} = gb, 0x17) do
    carry = CPU.read_flag(gb.cpu, :c)
    reg_val = CPU.read_register(gb.cpu, :a)
    # shift left
    reg_val_shift = (reg_val <<< 1 ||| reg_val >>> (8 - 1)) &&& 0b11111111
    # replace 0th bit with carry,
    reg_val_shift = write_bit(reg_val_shift, 0, carry)

    CPU.write_register(gb.cpu, :pc, CPU.read_register(gb.cpu, :pc) + 1)
    |> CPU.reset_flag(:z)
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.write_flag(:c, nth_bit(reg_val, 7))
    |> CPU.write_register(:a, reg_val_shift)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # JR r8
  def exec(%Gameboy{} = gb, 0x18) do
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1, :uint8)
    jmp = CPU.read_register(gb.cpu, :pc) + val + 2

    %{gb | cpu: CPU.write_register(gb.cpu, :pc, jmp)}
  end

  # ADD HL, DE
  # - 0 H C
  def exec(%Gameboy{} = gb, 0x19) do
    current_val = CPU.read_register(gb.cpu, :hl)
    val = CPU.read_register(gb.cpu, :de)
    [result, <<flags::integer>>] = Arithmetic.add16(current_val, val)

    z = CPU.read_flag(gb.cpu, :z)
    f = <<z::size(1), flags <<< 1::size(7)>>
    <<flags::integer>> = f

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:hl, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # LD A,(DE)
  def exec(%Gameboy{} = gb, 0x1A) do
    addr = CPU.read_register(gb.cpu, :de)
    data = Memory.read8(gb.memory, addr)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, data)

    %{gb | cpu: cpu}
  end

  # DEC DE
  def exec(%Gameboy{} = gb, 0x1B) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.dec_register(:de)

    %{gb | cpu: cpu}
  end

  # INC E
  def exec(%Gameboy{} = gb, 0x1C) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.inc_register(:e)

    %{gb | cpu: cpu}
  end

  # DEC E
  # Z 1 H -
  def exec(%Gameboy{} = gb, 0x1D) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.dec_register(:e)

    %{gb | cpu: cpu}
  end

  # LD E,d8
  def exec(%Gameboy{} = gb, 0x1E) do
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)

    cpu =
      gb.cpu
      |> CPU.inc_pc(2)
      |> CPU.write_register(:e, val)

    %{gb | cpu: cpu}
  end

  # JR NZ,r8
  def exec(%Gameboy{} = gb, 0x20) do
    cpu =
      case CPU.read_flag(gb.cpu, :z) do
        1 ->
          CPU.inc_pc(gb.cpu, 2)

        0 ->
          val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1, :uint8)
          jmp = CPU.read_register(gb.cpu, :pc) + val + 2
          CPU.write_register(gb.cpu, :pc, jmp)
      end

    %{gb | cpu: cpu}
  end

  # LD HL,d16
  def exec(%Gameboy{} = gb, 0x21) do
    val = Memory.read16(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)

    cpu =
      gb.cpu
      |> CPU.inc_pc(3)
      |> CPU.write_register(:hl, val)

    %{gb | cpu: cpu}
  end

  # LD (HL+),A
  def exec(%Gameboy{} = gb, 0x22) do
    addr = CPU.read_register(gb.cpu, :hl)
    data = CPU.read_register(gb.cpu, :a)
    memory = Memory.write8(gb.memory, addr, data)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.inc_register(:hl)

    %{gb | memory: memory, cpu: cpu}
  end

  # INC HL
  def exec(%Gameboy{} = gb, 0x23) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.inc_register(:hl)

    %{gb | cpu: cpu}
  end

  # INC H
  # Z 0 H -
  def exec(%Gameboy{} = gb, 0x24) do
    cpu =
      gb.cpu
      |> CPU.inc_register(:h)
      |> CPU.inc_pc()

    %{gb | cpu: cpu}
  end

  # DEC H
  # Z 0 H -
  def exec(%Gameboy{} = gb, 0x25) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.dec_register(:h)

    %{gb | cpu: cpu}
  end

  # LD H,d8
  def exec(%Gameboy{} = gb, 0x26) do
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)

    cpu =
      gb.cpu
      |> CPU.inc_pc(2)
      |> CPU.write_register(:h, val)

    %{gb | cpu: cpu}
  end

  # JR Z,r8
  def exec(%Gameboy{} = gb, 0x28) do
    cpu =
      case CPU.read_flag(gb.cpu, :z) do
        0 ->
          CPU.inc_pc(gb.cpu, 2)

        1 ->
          val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1, :uint8)
          jmp = CPU.read_register(gb.cpu, :pc) + val + 2
          CPU.write_register(gb.cpu, :pc, jmp)
      end

    %{gb | cpu: cpu}
  end

  # ADD HL, HL
  # - 0 H C
  def exec(%Gameboy{} = gb, 0x29) do
    current_val = CPU.read_register(gb.cpu, :hl)
    [result, <<flags::integer>>] = Arithmetic.add16(current_val, current_val)

    z = CPU.read_flag(gb.cpu, :z)
    f = <<z::size(1), flags <<< 1::size(7)>>
    <<flags::integer>> = f

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:hl, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # LD A,(HL+)
  def exec(%Gameboy{} = gb, 0x2A) do
    addr = CPU.read_register(gb.cpu, :hl)
    data = Memory.read8(gb.memory, addr)

    cpu =
      gb.cpu
      |> CPU.write_register(:a, data)
      |> CPU.inc_pc()
      |> CPU.inc_register(:hl)

    %{gb | cpu: cpu}
  end

  # DEC HL
  def exec(%Gameboy{} = gb, 0x2B) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.dec_register(:hl)

    %{gb | cpu: cpu}
  end

  # INC L
  def exec(%Gameboy{} = gb, 0x2C) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.inc_register(:l)

    %{gb | cpu: cpu}
  end

  # DEC L
  def exec(%Gameboy{} = gb, 0x2D) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.dec_register(:l)

    %{gb | cpu: cpu}
  end

  # LD L,d8
  def exec(%Gameboy{} = gb, 0x2E) do
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)

    cpu =
      gb.cpu
      |> CPU.inc_pc(2)
      |> CPU.write_register(:l, val)

    %{gb | cpu: cpu}
  end

  # CPL
  # - 1 1 -
  def exec(%Gameboy{} = gb, 0x2F) do
    val =
      gb.cpu
      |> CPU.read_register(:a)
      |> bnot()
      |> band(0xFF)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, val)
      |> CPU.set_flag(:n)
      |> CPU.set_flag(:h)

    %{gb | cpu: cpu}
  end

  # JR NC,r8
  def exec(%Gameboy{} = gb, 0x30) do
    cpu =
      case CPU.read_flag(gb.cpu, :c) do
        1 ->
          CPU.inc_pc(gb.cpu, 2)

        0 ->
          val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1, :uint8)
          jmp = CPU.read_register(gb.cpu, :pc) + val + 2
          CPU.write_register(gb.cpu, :pc, jmp)
      end

    %{gb | cpu: cpu}
  end

  # LD SP,d16
  def exec(%Gameboy{} = gb, 0x31) do
    val = Memory.read16(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)

    cpu =
      gb.cpu
      |> CPU.inc_pc(3)
      |> CPU.write_register(:sp, val)

    %{gb | cpu: cpu}
  end

  # LD (HL-),A
  def exec(%Gameboy{} = gb, 0x32) do
    addr = CPU.read_register(gb.cpu, :hl)
    memory = Memory.write8(gb.memory, addr, CPU.read_register(gb.cpu, :a))

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:hl, addr - 1)

    %{gb | memory: memory, cpu: cpu}
  end

  # INC SP
  def exec(%Gameboy{} = gb, 0x33) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.inc_register(:sp)

    %{gb | cpu: cpu}
  end

  # INC (HL)
  # Z 0 H -
  def exec(%Gameboy{} = gb, 0x34) do
    addr = CPU.read_register(gb.cpu, :hl)
    val = Memory.read8(gb.memory, addr)
    new_val = band(0xFF, val + 1)
    memory = Memory.write8(gb.memory, addr, new_val)

    cpu = if new_val == 0, do: CPU.set_flag(gb.cpu, :z), else: CPU.reset_flag(gb.cpu, :z)
    cpu = if nth_bit(new_val, 3) == 1, do: CPU.set_flag(cpu, :h), else: CPU.reset_flag(cpu, :h)

    cpu =
      cpu
      |> CPU.reset_flag(:n)
      |> CPU.inc_pc()

    %{gb | cpu: cpu, memory: memory}
  end

  # LD (HL),d8
  def exec(%Gameboy{} = gb, 0x36) do
    addr = CPU.read_register(gb.cpu, :hl)
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
    memory = Memory.write8(gb.memory, addr, val)

    %{gb | memory: memory, cpu: CPU.inc_pc(gb.cpu, 2)}
  end

  # SCF
  # - 0 0 1
  def exec(%Gameboy{} = gb, 0x37) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.reset_flag(:n)
      |> CPU.reset_flag(:h)
      |> CPU.set_flag(:c)

    %{gb | cpu: cpu}
  end

  # JR C,r8
  def exec(%Gameboy{} = gb, 0x38) do
    cpu =
      case CPU.read_flag(gb.cpu, :c) do
        0 ->
          CPU.inc_pc(gb.cpu, 2)

        1 ->
          val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1, :uint8)
          jmp = CPU.read_register(gb.cpu, :pc) + val + 2
          CPU.write_register(gb.cpu, :pc, jmp)
      end

    %{gb | cpu: cpu}
  end

  # ADD HL, SP
  # - 0 H C
  def exec(%Gameboy{} = gb, 0x39) do
    current_val = CPU.read_register(gb.cpu, :hl)
    val = CPU.read_register(gb.cpu, :sp)

    [result, <<flags::integer>>] = Arithmetic.add16(current_val, val)

    z = CPU.read_flag(gb.cpu, :z)
    f = <<z::size(1), flags <<< 1::size(7)>>
    <<flags::integer>> = f

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:hl, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # DEC SP
  def exec(%Gameboy{} = gb, 0x3B) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.dec_register(:sp)

    %{gb | cpu: cpu}
  end

  # INC A
  # Z 0 H -
  def exec(%Gameboy{} = gb, 0x3C) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.inc_register(:a)

    %{gb | cpu: cpu}
  end

  # DEC A
  # Z 1 H -
  def exec(%Gameboy{} = gb, 0x3D) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.dec_register(:a)

    %{gb | cpu: cpu}
  end

  # LD A,d8
  def exec(%Gameboy{} = gb, 0x3E) do
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)

    cpu =
      gb.cpu
      |> CPU.inc_pc(2)
      |> CPU.write_register(:a, val)

    %{gb | cpu: cpu}
  end

  # CCF
  def exec(%Gameboy{} = gb, 0x3F) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.reset_flag(:n)
      |> CPU.reset_flag(:h)
      |> (fn cpu ->
            if CPU.read_flag(cpu, :c) == 1 do
              CPU.reset_flag(cpu, :c)
            else
              CPU.set_flag(cpu, :c)
            end
          end).()

    %{gb | cpu: cpu}
  end

  # LD B,B
  def exec(%Gameboy{} = gb, 0x40) do
    cpu = Load.ld(gb.cpu, :b, :b)

    %{gb | cpu: cpu}
  end

  # LD B,C
  def exec(%Gameboy{} = gb, 0x41) do
    cpu = Load.ld(gb.cpu, :b, :c)

    %{gb | cpu: cpu}
  end

  # LD B,D
  def exec(%Gameboy{} = gb, 0x42) do
    cpu = Load.ld(gb.cpu, :b, :d)

    %{gb | cpu: cpu}
  end

  # LD B,E
  def exec(%Gameboy{} = gb, 0x43) do
    cpu = Load.ld(gb.cpu, :b, :e)

    %{gb | cpu: cpu}
  end

  # LD B,H
  def exec(%Gameboy{} = gb, 0x44) do
    cpu = Load.ld(gb.cpu, :b, :h)

    %{gb | cpu: cpu}
  end

  # LD B,L
  def exec(%Gameboy{} = gb, 0x45) do
    cpu = Load.ld(gb.cpu, :b, :l)

    %{gb | cpu: cpu}
  end

  # LD B,(HL)
  def exec(%Gameboy{} = gb, 0x46) do
    addr = CPU.read_register(gb.cpu, :hl)
    data = Memory.read8(gb.memory, addr)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:b, data)

    %{gb | cpu: cpu}
  end

  # LD B,A
  def exec(%Gameboy{} = gb, 0x47) do
    cpu = Load.ld(gb.cpu, :b, :a)

    %{gb | cpu: cpu}
  end

  # LD C,B
  def exec(%Gameboy{} = gb, 0x48) do
    cpu = Load.ld(gb.cpu, :c, :b)

    %{gb | cpu: cpu}
  end

  # LD C,C
  def exec(%Gameboy{} = gb, 0x49) do
    cpu = Load.ld(gb.cpu, :c, :c)

    %{gb | cpu: cpu}
  end

  # LD C,D
  def exec(%Gameboy{} = gb, 0x4A) do
    cpu = Load.ld(gb.cpu, :c, :d)

    %{gb | cpu: cpu}
  end

  # LD C,E
  def exec(%Gameboy{} = gb, 0x4B) do
    cpu = Load.ld(gb.cpu, :c, :e)

    %{gb | cpu: cpu}
  end

  # LD C,H
  def exec(%Gameboy{} = gb, 0x4C) do
    cpu = Load.ld(gb.cpu, :c, :h)

    %{gb | cpu: cpu}
  end

  # LD C,L
  def exec(%Gameboy{} = gb, 0x4D) do
    cpu = Load.ld(gb.cpu, :c, :l)

    %{gb | cpu: cpu}
  end

  # LD C,(HL)
  def exec(%Gameboy{} = gb, 0x4E) do
    addr = CPU.read_register(gb.cpu, :hl)
    data = Memory.read8(gb.memory, addr)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:c, data)

    %{gb | cpu: cpu}
  end

  # LD C,A
  def exec(%Gameboy{} = gb, 0x4F) do
    cpu = Load.ld(gb.cpu, :c, :a)

    %{gb | cpu: cpu}
  end

  # LD D,B
  def exec(%Gameboy{} = gb, 0x50) do
    cpu = Load.ld(gb.cpu, :d, :b)

    %{gb | cpu: cpu}
  end

  # LD D,C
  def exec(%Gameboy{} = gb, 0x51) do
    cpu = Load.ld(gb.cpu, :d, :c)

    %{gb | cpu: cpu}
  end

  # LD D,D
  def exec(%Gameboy{} = gb, 0x52) do
    cpu = Load.ld(gb.cpu, :d, :d)

    %{gb | cpu: cpu}
  end

  # LD D,E
  def exec(%Gameboy{} = gb, 0x53) do
    cpu = Load.ld(gb.cpu, :d, :e)

    %{gb | cpu: cpu}
  end

  # LD D,H
  def exec(%Gameboy{} = gb, 0x54) do
    cpu = Load.ld(gb.cpu, :d, :h)

    %{gb | cpu: cpu}
  end

  # LD D,L
  def exec(%Gameboy{} = gb, 0x55) do
    cpu = Load.ld(gb.cpu, :d, :l)

    %{gb | cpu: cpu}
  end

  # LD D,(HL)
  def exec(%Gameboy{} = gb, 0x56) do
    addr = CPU.read_register(gb.cpu, :hl)
    data = Memory.read8(gb.memory, addr)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:d, data)

    %{gb | cpu: cpu}
  end

  # LD D,A
  def exec(%Gameboy{} = gb, 0x57) do
    cpu = Load.ld(gb.cpu, :d, :a)

    %{gb | cpu: cpu}
  end

  # LD E,B
  def exec(%Gameboy{} = gb, 0x58) do
    cpu = Load.ld(gb.cpu, :e, :b)

    %{gb | cpu: cpu}
  end

  # LD E,C
  def exec(%Gameboy{} = gb, 0x59) do
    cpu = Load.ld(gb.cpu, :e, :c)

    %{gb | cpu: cpu}
  end

  # LD E,D
  def exec(%Gameboy{} = gb, 0x5A) do
    cpu = Load.ld(gb.cpu, :e, :d)

    %{gb | cpu: cpu}
  end

  # LD E,E
  def exec(%Gameboy{} = gb, 0x5B) do
    cpu = Load.ld(gb.cpu, :e, :e)

    %{gb | cpu: cpu}
  end

  # LD E,H
  def exec(%Gameboy{} = gb, 0x5C) do
    cpu = Load.ld(gb.cpu, :e, :h)

    %{gb | cpu: cpu}
  end

  # LD E,L
  def exec(%Gameboy{} = gb, 0x5D) do
    cpu = Load.ld(gb.cpu, :e, :l)

    %{gb | cpu: cpu}
  end

  # LD E,(HL)
  def exec(%Gameboy{} = gb, 0x5E) do
    addr = CPU.read_register(gb.cpu, :hl)
    data = Memory.read8(gb.memory, addr)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:e, data)

    %{gb | cpu: cpu}
  end

  # LD E,A
  def exec(%Gameboy{} = gb, 0x5F) do
    a = CPU.read_register(gb.cpu, :a)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:e, a)

    %{gb | cpu: cpu}
  end

  # LD H,B
  def exec(%Gameboy{} = gb, 0x60) do
    cpu = Load.ld(gb.cpu, :h, :b)

    %{gb | cpu: cpu}
  end

  # LD H,C
  def exec(%Gameboy{} = gb, 0x61) do
    cpu = Load.ld(gb.cpu, :h, :c)

    %{gb | cpu: cpu}
  end

  # LD H,D
  def exec(%Gameboy{} = gb, 0x62) do
    cpu = Load.ld(gb.cpu, :h, :d)

    %{gb | cpu: cpu}
  end

  # LD H,E
  def exec(%Gameboy{} = gb, 0x63) do
    cpu = Load.ld(gb.cpu, :h, :e)

    %{gb | cpu: cpu}
  end

  # LD H,H
  def exec(%Gameboy{} = gb, 0x64) do
    cpu = Load.ld(gb.cpu, :h, :h)

    %{gb | cpu: cpu}
  end

  # LD H,L
  def exec(%Gameboy{} = gb, 0x65) do
    cpu = Load.ld(gb.cpu, :h, :l)

    %{gb | cpu: cpu}
  end

  # LD L,(HL)
  # def exec(%Gameboy{} = gb, 0x66) do
  # cpu = Load.ld(gb.cpu, :l, :hl)

  # %{gb | cpu: cpu}
  # end

  # LD L,(HL)
  def exec(%Gameboy{} = gb, 0x66) do
    addr = CPU.read_register(gb.cpu, :hl)
    val = Memory.read8(gb.memory, addr)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:l, val)

    %{gb | cpu: cpu}
  end

  # LD H,A
  def exec(%Gameboy{} = gb, 0x67) do
    cpu = Load.ld(gb.cpu, :h, :a)

    %{gb | cpu: cpu}
  end

  # LD L,B
  def exec(%Gameboy{} = gb, 0x68) do
    cpu = Load.ld(gb.cpu, :l, :b)

    %{gb | cpu: cpu}
  end

  # LD L,C
  def exec(%Gameboy{} = gb, 0x69) do
    cpu = Load.ld(gb.cpu, :l, :c)

    %{gb | cpu: cpu}
  end

  # LD L,D
  def exec(%Gameboy{} = gb, 0x6A) do
    cpu = Load.ld(gb.cpu, :l, :d)

    %{gb | cpu: cpu}
  end

  # LD L,E
  def exec(%Gameboy{} = gb, 0x6B) do
    cpu = Load.ld(gb.cpu, :l, :e)

    %{gb | cpu: cpu}
  end

  # LD L,H
  def exec(%Gameboy{} = gb, 0x6C) do
    cpu = Load.ld(gb.cpu, :l, :h)

    %{gb | cpu: cpu}
  end

  # LD L,L
  def exec(%Gameboy{} = gb, 0x6D) do
    cpu = Load.ld(gb.cpu, :l, :l)

    %{gb | cpu: cpu}
  end

  # LD L,(HL)
  def exec(%Gameboy{} = gb, 0x6E) do
    addr = CPU.read_register(gb.cpu, :hl)
    data = Memory.read8(gb.memory, addr)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:l, data)

    %{gb | cpu: cpu}
  end

  # LD L,A
  def exec(%Gameboy{} = gb, 0x6F) do
    cpu = Load.ld(gb.cpu, :l, :a)

    %{gb | cpu: cpu}
  end

  # LD (HL),B
  def exec(%Gameboy{} = gb, 0x70) do
    Load.ld(gb, :hl, :b)
  end

  # LD (HL),C
  def exec(%Gameboy{} = gb, 0x71) do
    Load.ld(gb, :hl, :c)
  end

  # LD (HL),D
  def exec(%Gameboy{} = gb, 0x72) do
    Load.ld(gb, :hl, :d)
  end

  # LD (HL),E
  def exec(%Gameboy{} = gb, 0x73) do
    Load.ld(gb, :hl, :e)
  end

  # LD (HL),H
  def exec(%Gameboy{} = gb, 0x74) do
    Load.ld(gb, :hl, :h)
  end

  # LD (HL),L
  def exec(%Gameboy{} = gb, 0x75) do
    Load.ld(gb, :hl, :l)
  end

  # HALT
  def exec(%Gameboy{} = gb, 0x76) do
    # TODO: implement
    gb
  end

  # LD (HL),A
  def exec(%Gameboy{} = gb, 0x77) do
    cpu = CPU.write_register(gb.cpu, :pc, CPU.read_register(gb.cpu, :pc) + 1)
    addr = CPU.read_register(cpu, :hl)
    memory = Memory.write8(gb.memory, addr, CPU.read_register(gb.cpu, :a))

    %{gb | memory: memory, cpu: cpu}
  end

  # LD A,B
  def exec(%Gameboy{} = gb, 0x78) do
    CPU.write_register(gb.cpu, :pc, CPU.read_register(gb.cpu, :pc) + 1)
    |> CPU.write_register(:a, CPU.read_register(gb.cpu, :b))
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # LD A,C
  def exec(%Gameboy{} = gb, 0x79) do
    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, CPU.read_register(gb.cpu, :c))

    %{gb | cpu: cpu}
  end

  # LD A,D
  def exec(%Gameboy{} = gb, 0x7A) do
    cpu = Load.ld(gb.cpu, :a, :d)

    %{gb | cpu: cpu}
  end

  # LD A,E
  def exec(%Gameboy{} = gb, 0x7B) do
    cpu = Load.ld(gb.cpu, :a, :e)

    %{gb | cpu: cpu}
  end

  # LD A,H
  def exec(%Gameboy{} = gb, 0x7C) do
    cpu = Load.ld(gb.cpu, :a, :h)

    %{gb | cpu: cpu}
  end

  # LD A,L
  def exec(%Gameboy{} = gb, 0x7D) do
    cpu = Load.ld(gb.cpu, :a, :l)

    %{gb | cpu: cpu}
  end

  # LD A,(HL)
  def exec(%Gameboy{} = gb, 0x7E) do
    cpu = CPU.write_register(gb.cpu, :pc, CPU.read_register(gb.cpu, :pc) + 1)
    addr = CPU.read_register(cpu, :hl)
    data = Memory.read8(gb.memory, addr)

    CPU.write_register(cpu, :a, data)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # LD A,A
  def exec(%Gameboy{} = gb, 0x7F) do
    cpu = Load.ld(gb.cpu, :a, :a)

    %{gb | cpu: cpu}
  end

  # ADD A,B
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x80) do
    current_val = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :b)
    [result, <<flags::integer>>] = Arithmetic.add(current_val, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # ADD A,C
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x81) do
    current_val = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :c)
    [result, <<flags::integer>>] = Arithmetic.add(current_val, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # ADD A,D
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x82) do
    current_val = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :d)
    [result, <<flags::integer>>] = Arithmetic.add(current_val, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # ADD A,E
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x83) do
    current_val = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :e)
    [result, <<flags::integer>>] = Arithmetic.add(current_val, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # ADD A,H
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x84) do
    current_val = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :h)
    [result, <<flags::integer>>] = Arithmetic.add(current_val, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # ADD A,L
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x85) do
    current_val = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :l)
    [result, <<flags::integer>>] = Arithmetic.add(current_val, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # ADD A,(HL)
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x86) do
    current_val = CPU.read_register(gb.cpu, :a)
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :hl))
    [result, <<flags::integer>>] = Arithmetic.add(current_val, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    # new_val = current_val + val
    # cpu = CPU.write_register(gb.cpu, :pc, CPU.read_register(gb.cpu, :pc) + 1)
    #   |> CPU.write_register(:a, band(0xFF, new_val))
    #   |> CPU.reset_flag(:n)

    # cpu = if new_val == 0, do: CPU.set_flag(cpu, :z), else: CPU.reset_flag(cpu, :z)
    # cpu = if new_val > 0xFF, do: CPU.set_flag(cpu, :h), else: CPU.reset_flag(cpu, :h)
    # cpu = if nth_bit(new_val, 7) == 1, do: CPU.set_flag(cpu, :c), else: CPU.reset_flag(cpu, :c)
    %{gb | cpu: cpu}
  end

  # ADD A,A
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x87) do
    val = CPU.read_register(gb.cpu, :a)
    [result, <<flags::integer>>] = Arithmetic.add(val, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # ADC A,B
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x88) do
    current = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :b)
    [result, <<flags::integer>>] = Arithmetic.addc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # ADC A,C
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x89) do
    current = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :c)
    [result, <<flags::integer>>] = Arithmetic.addc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # ADC A,D
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x8A) do
    current = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :d)
    [result, <<flags::integer>>] = Arithmetic.addc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # ADC A,E
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x8B) do
    current = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :e)
    [result, <<flags::integer>>] = Arithmetic.addc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # ADC A,H
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x8C) do
    current = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :h)
    [result, <<flags::integer>>] = Arithmetic.addc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # ADC A,L
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x8D) do
    current = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :l)
    [result, <<flags::integer>>] = Arithmetic.addc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # ADC A,(HL)
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x8E) do
    current = CPU.read_register(gb.cpu, :a)
    addr = CPU.read_register(gb.cpu, :hl)
    val = Memory.read8(gb.memory, addr)
    [result, <<flags::integer>>] = Arithmetic.addc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # ADC A,A
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0x8F) do
    val = CPU.read_register(gb.cpu, :a)
    [result, <<flags::integer>>] = Arithmetic.addc(gb.cpu, val, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SUB B
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x90) do
    [res, <<flags::integer>>] =
      Arithmetic.sub(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :b))

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, res)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SUB C @TODO
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x91) do
    [res, <<flags::integer>>] =
      Arithmetic.sub(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :c))

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, res)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SUB D @TODO
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x92) do
    [res, <<flags::integer>>] =
      Arithmetic.sub(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :d))

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, res)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SUB E @TODO
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x93) do
    [res, <<flags::integer>>] =
      Arithmetic.sub(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :e))

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, res)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SUB H @TODO
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x94) do
    [res, <<flags::integer>>] =
      Arithmetic.sub(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :h))

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, res)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SUB L @TODO
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x95) do
    [res, <<flags::integer>>] =
      Arithmetic.sub(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :l))

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, res)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SUB (HL) @TODO: (HL) means read at HL, not from HL..?
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x96) do
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :hl))
    [res, <<flags::integer>>] = Arithmetic.sub(CPU.read_register(gb.cpu, :a), val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, res)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SUB A
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x97) do
    val = CPU.read_register(gb.cpu, :a)
    [res, <<flags::integer>>] = Arithmetic.sub(val, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, res)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SBC A,B
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x98) do
    current = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :b)
    [result, <<flags::integer>>] = Arithmetic.subc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SBC A,C
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x99) do
    current = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :c)
    [result, <<flags::integer>>] = Arithmetic.subc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SBC A,D
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x9A) do
    current = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :d)
    [result, <<flags::integer>>] = Arithmetic.subc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SBC A,E
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x9B) do
    current = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :e)
    [result, <<flags::integer>>] = Arithmetic.subc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SBC A,H
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x9C) do
    current = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :h)
    [result, <<flags::integer>>] = Arithmetic.subc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SBC A,L
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x9D) do
    current = CPU.read_register(gb.cpu, :a)
    val = CPU.read_register(gb.cpu, :l)
    [result, <<flags::integer>>] = Arithmetic.subc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SBC A,(HL)
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x9E) do
    current = CPU.read_register(gb.cpu, :a)
    addr = CPU.read_register(gb.cpu, :hl)
    val = Memory.read8(gb.memory, addr)
    [result, <<flags::integer>>] = Arithmetic.subc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # SBC A,A
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0x9F) do
    val = CPU.read_register(gb.cpu, :a)
    [result, <<flags::integer>>] = Arithmetic.subc(gb.cpu, val, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # AND B
  # Z 0 1 0
  def exec(%Gameboy{} = gb, 0xA0) do
    val =
      CPU.read_register(gb.cpu, :a)
      |> band(CPU.read_register(gb.cpu, :b))
      |> band(0xFF)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, val)
      |> (fn cpu ->
            if val == 0 do
              CPU.set_flag(cpu, :z)
            else
              CPU.reset_flag(cpu, :z)
            end
          end).()
      |> CPU.reset_flag(:n)
      |> CPU.set_flag(:h)
      |> CPU.reset_flag(:c)

    %{gb | cpu: cpu}
  end

  # AND C
  # Z 0 1 0
  def exec(%Gameboy{} = gb, 0xA1) do
    val =
      CPU.read_register(gb.cpu, :a)
      |> band(CPU.read_register(gb.cpu, :c))
      |> band(0xFF)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, val)
      |> (fn cpu ->
            if val == 0 do
              CPU.set_flag(cpu, :z)
            else
              CPU.reset_flag(cpu, :z)
            end
          end).()
      |> CPU.reset_flag(:n)
      |> CPU.set_flag(:h)
      |> CPU.reset_flag(:c)

    %{gb | cpu: cpu}
  end

  # AND D
  # Z 0 1 0
  def exec(%Gameboy{} = gb, 0xA2) do
    val =
      CPU.read_register(gb.cpu, :a)
      |> band(CPU.read_register(gb.cpu, :d))
      |> band(0xFF)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, val)
      |> (fn cpu ->
            if val == 0 do
              CPU.set_flag(cpu, :z)
            else
              CPU.reset_flag(cpu, :z)
            end
          end).()
      |> CPU.reset_flag(:n)
      |> CPU.set_flag(:h)
      |> CPU.reset_flag(:c)

    %{gb | cpu: cpu}
  end

  # AND E
  # Z 0 1 0
  def exec(%Gameboy{} = gb, 0xA3) do
    val =
      CPU.read_register(gb.cpu, :a)
      |> band(CPU.read_register(gb.cpu, :e))
      |> band(0xFF)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, val)
      |> (fn cpu ->
            if val == 0 do
              CPU.set_flag(cpu, :z)
            else
              CPU.reset_flag(cpu, :z)
            end
          end).()
      |> CPU.reset_flag(:n)
      |> CPU.set_flag(:h)
      |> CPU.reset_flag(:c)

    %{gb | cpu: cpu}
  end

  # AND H
  # Z 0 1 0
  def exec(%Gameboy{} = gb, 0xA4) do
    val =
      CPU.read_register(gb.cpu, :a)
      |> band(CPU.read_register(gb.cpu, :h))
      |> band(0xFF)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, val)
      |> (fn cpu ->
            if val == 0 do
              CPU.set_flag(cpu, :z)
            else
              CPU.reset_flag(cpu, :z)
            end
          end).()
      |> CPU.reset_flag(:n)
      |> CPU.set_flag(:h)
      |> CPU.reset_flag(:c)

    %{gb | cpu: cpu}
  end

  # AND L
  # Z 0 1 0
  def exec(%Gameboy{} = gb, 0xA5) do
    val =
      CPU.read_register(gb.cpu, :a)
      |> band(CPU.read_register(gb.cpu, :l))
      |> band(0xFF)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, val)
      |> (fn cpu ->
            if val == 0 do
              CPU.set_flag(cpu, :z)
            else
              CPU.reset_flag(cpu, :z)
            end
          end).()
      |> CPU.reset_flag(:n)
      |> CPU.set_flag(:h)
      |> CPU.reset_flag(:c)

    %{gb | cpu: cpu}
  end

  # AND (HL)
  # Z 0 1 0
  def exec(%Gameboy{} = gb, 0xA6) do
    val =
      gb.memory
      |> Memory.read8(CPU.read_register(gb.cpu, :hl))
      |> band(CPU.read_register(gb.cpu, :a))
      |> band(0xFF)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, val)
      |> (fn cpu ->
            if val == 0 do
              CPU.set_flag(cpu, :z)
            else
              CPU.reset_flag(cpu, :z)
            end
          end).()
      |> CPU.reset_flag(:n)
      |> CPU.set_flag(:h)
      |> CPU.reset_flag(:c)

    %{gb | cpu: cpu}
  end

  # AND A
  # Z 0 1 0
  def exec(%Gameboy{} = gb, 0xA7) do
    val =
      CPU.read_register(gb.cpu, :a)
      |> band(CPU.read_register(gb.cpu, :a))
      |> band(0xFF)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, val)
      |> (fn cpu ->
            if val == 0 do
              CPU.set_flag(cpu, :z)
            else
              CPU.reset_flag(cpu, :z)
            end
          end).()
      |> CPU.reset_flag(:n)
      |> CPU.set_flag(:h)
      |> CPU.reset_flag(:c)

    %{gb | cpu: cpu}
  end

  # XOR B
  def exec(%Gameboy{} = gb, 0xA8) do
    val = band(0xFF, bxor(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :b)))

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # XOR C
  def exec(%Gameboy{} = gb, 0xA9) do
    val = band(0xFF, bxor(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :b)))

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # XOR D
  def exec(%Gameboy{} = gb, 0xAA) do
    val = band(0xFF, bxor(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :d)))

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # XOR E
  def exec(%Gameboy{} = gb, 0xAB) do
    val = band(0xFF, bxor(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :e)))

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # XOR H
  def exec(%Gameboy{} = gb, 0xAC) do
    val = band(0xFF, bxor(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :h)))

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # XOR L
  def exec(%Gameboy{} = gb, 0xAD) do
    val = band(0xFF, bxor(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :l)))

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # XOR (HL)
  def exec(%Gameboy{} = gb, 0xAE) do
    data = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :hl))
    val = band(0xFF, bxor(CPU.read_register(gb.cpu, :a), data))

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # XOR A
  def exec(%Gameboy{} = gb, 0xAF) do
    val = band(0xFF, bxor(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :a)))

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # OR B
  # Z 0 0 0
  def exec(%Gameboy{} = gb, 0xB0) do
    val = CPU.read_register(gb.cpu, :a) ||| CPU.read_register(gb.cpu, :b)

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # OR C
  # Z 0 0 0
  def exec(%Gameboy{} = gb, 0xB1) do
    val = CPU.read_register(gb.cpu, :a) ||| CPU.read_register(gb.cpu, :c)

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # OR D
  # Z 0 0 0
  def exec(%Gameboy{} = gb, 0xB2) do
    val = CPU.read_register(gb.cpu, :a) ||| CPU.read_register(gb.cpu, :d)

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # OR E
  # Z 0 0 0
  def exec(%Gameboy{} = gb, 0xB3) do
    val = CPU.read_register(gb.cpu, :a) ||| CPU.read_register(gb.cpu, :e)

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # OR H
  # Z 0 0 0
  def exec(%Gameboy{} = gb, 0xB4) do
    val = CPU.read_register(gb.cpu, :a) ||| CPU.read_register(gb.cpu, :h)

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # OR L
  # Z 0 0 0
  def exec(%Gameboy{} = gb, 0xB5) do
    val = CPU.read_register(gb.cpu, :a) ||| CPU.read_register(gb.cpu, :l)

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # OR (HL)
  # Z 0 0 0
  def exec(%Gameboy{} = gb, 0xB6) do
    data = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :hl))
    val = CPU.read_register(gb.cpu, :a) ||| data

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # OR A
  # Z 0 0 0
  def exec(%Gameboy{} = gb, 0xB7) do
    val = CPU.read_register(gb.cpu, :a) ||| CPU.read_register(gb.cpu, :a)

    gb.cpu
    |> CPU.inc_pc()
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # CP B
  def exec(%Gameboy{} = gb, 0xB8) do
    [_res, <<flags::integer>>] =
      Arithmetic.sub(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :b))

    cpu =
      gb.cpu
      |> CPU.write_register(:f, flags)
      |> CPU.inc_pc()

    %{gb | cpu: cpu}
  end

  # CP C
  def exec(%Gameboy{} = gb, 0xB9) do
    [_res, <<flags::integer>>] =
      Arithmetic.sub(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :c))

    cpu =
      gb.cpu
      |> CPU.write_register(:f, flags)
      |> CPU.inc_pc()

    %{gb | cpu: cpu}
  end

  # CP D
  def exec(%Gameboy{} = gb, 0xBA) do
    [_res, <<flags::integer>>] =
      Arithmetic.sub(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :d))

    cpu =
      gb.cpu
      |> CPU.write_register(:f, flags)
      |> CPU.inc_pc()

    %{gb | cpu: cpu}
  end

  # CP E
  def exec(%Gameboy{} = gb, 0xBB) do
    [_res, <<flags::integer>>] =
      Arithmetic.sub(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :e))

    cpu =
      gb.cpu
      |> CPU.write_register(:f, flags)
      |> CPU.inc_pc()

    %{gb | cpu: cpu}
  end

  # CP H
  def exec(%Gameboy{} = gb, 0xBC) do
    [_res, <<flags::integer>>] =
      Arithmetic.sub(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :h))

    cpu =
      gb.cpu
      |> CPU.write_register(:f, flags)
      |> CPU.inc_pc()

    %{gb | cpu: cpu}
  end

  # CP L
  def exec(%Gameboy{} = gb, 0xBD) do
    [_res, <<flags::integer>>] =
      Arithmetic.sub(CPU.read_register(gb.cpu, :a), CPU.read_register(gb.cpu, :l))

    cpu =
      gb.cpu
      |> CPU.write_register(:f, flags)
      |> CPU.inc_pc()

    %{gb | cpu: cpu}
  end

  # CP (HL)
  def exec(%Gameboy{} = gb, 0xBE) do
    a = CPU.read_register(gb.cpu, :a)
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :hl))

    [_res, <<flags::integer>>] = Arithmetic.sub(a, val)

    cpu =
      gb.cpu
      |> CPU.write_register(:f, flags)
      |> CPU.inc_pc()

    %{gb | cpu: cpu}
  end

  # CP A
  def exec(%Gameboy{} = gb, 0xBF) do
    a = CPU.read_register(gb.cpu, :a)
    [_res, <<flags::integer>>] = Arithmetic.sub(a, a)

    cpu =
      gb.cpu
      |> CPU.write_register(:f, flags)
      |> CPU.inc_pc()

    %{gb | cpu: cpu}
  end

  # RET NZ
  def exec(%Gameboy{} = gb, 0xC0) do
    cpu =
      case CPU.read_flag(gb.cpu, :z) do
        0 ->
          sp = CPU.read_register(gb.cpu, :sp)
          val = Memory.read16(gb.memory, sp)

          gb.cpu
          |> CPU.write_register(:pc, val)
          |> CPU.write_register(:sp, sp + 2)

        1 ->
          CPU.inc_pc(gb.cpu, 1)
      end

    %{gb | cpu: cpu}
  end

  # POP BC
  @todo "double check this"
  def exec(%Gameboy{} = gb, 0xC1) do
    sp = CPU.read_register(gb.cpu, :sp)
    data = Memory.read16(gb.memory, sp)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:bc, data)
      |> CPU.write_register(:sp, sp + 2)

    %{gb | cpu: cpu}
  end

  # JP NZ, a16
  def exec(%Gameboy{} = gb, 0xC2) do
    cpu =
      case CPU.read_flag(gb.cpu, :z) do
        0 ->
          val = Memory.read16(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
          CPU.write_register(gb.cpu, :pc, val)

        1 ->
          CPU.inc_pc(gb.cpu, 3)
      end

    %{gb | cpu: cpu}
  end

  def exec(%Gameboy{cpu: %{pc: %{value: 0x02BC}}} = gb, 0xC3) do
    pc = CPU.read_register(gb.cpu, :pc)
    {pch, pcl} = split_uint16(pc)

    addrh = CPU.read_register(gb.cpu, :sp) - 1
    addrl = CPU.read_register(gb.cpu, :sp) - 2

    memory =
      Memory.write8(gb.memory, addrh, pch)
      |> Memory.write8(addrl, pcl)

    cpu =
      CPU.write_register(gb.cpu, :sp, addrl)
      |> CPU.write_register(:pc, 0x0040)

    %{gb | cpu: cpu, memory: memory}
  end

  # JP a16
  def exec(%Gameboy{} = gb, 0xC3) do
    val = Memory.read16(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
    cpu = CPU.write_register(gb.cpu, :pc, val)

    %{gb | cpu: cpu}
  end

  # CALL NZ, a16
  def exec(%Gameboy{} = gb, 0xC4) do
    case CPU.read_flag(gb.cpu, :z) do
      1 ->
        %{gb | cpu: CPU.inc_pc(gb.cpu, 3)}

      0 ->
        val = Memory.read16(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
        pc = CPU.read_register(gb.cpu, :pc) + 3
        {pch, pcl} = split_uint16(pc)

        addrh = CPU.read_register(gb.cpu, :sp) - 1
        addrl = CPU.read_register(gb.cpu, :sp) - 2

        memory =
          Memory.write8(gb.memory, addrh, pch)
          |> Memory.write8(addrl, pcl)

        cpu =
          CPU.write_register(gb.cpu, :sp, addrl)
          |> CPU.write_register(:pc, val)

        %{gb | cpu: cpu, memory: memory}
    end
  end

  # PUSH BC
  @todo "double check this"
  def exec(%Gameboy{} = gb, 0xC5) do
    val = CPU.read_register(gb.cpu, :bc)
    {pch, pcl} = split_uint16(val)

    addrh = CPU.read_register(gb.cpu, :sp) - 1
    addrl = CPU.read_register(gb.cpu, :sp) - 2

    memory =
      gb.memory
      |> Memory.write8(addrh, pch)
      |> Memory.write8(addrl, pcl)

    cpu =
      gb.cpu
      |> CPU.write_register(:sp, addrl)
      |> CPU.inc_pc(1)

    %{gb | cpu: cpu, memory: memory}
  end

  # ADD A,d8
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0xC6) do
    current_val = CPU.read_register(gb.cpu, :a)
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
    [result, <<flags::integer>>] = Arithmetic.add(current_val, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # RET Z
  def exec(%Gameboy{} = gb, 0xC8) do
    cpu =
      case CPU.read_flag(gb.cpu, :z) do
        1 ->
          sp = CPU.read_register(gb.cpu, :sp)
          val = Memory.read16(gb.memory, sp)

          gb.cpu
          |> CPU.write_register(:pc, val)
          |> CPU.write_register(:sp, sp + 2)

        0 ->
          CPU.inc_pc(gb.cpu, 1)
      end

    %{gb | cpu: cpu}
  end

  # RET
  def exec(%Gameboy{} = gb, 0xC9) do
    sp = CPU.read_register(gb.cpu, :sp)
    val = Memory.read16(gb.memory, sp)

    cpu =
      gb.cpu
      |> CPU.write_register(:pc, val)
      |> CPU.write_register(:sp, sp + 2)

    %{gb | cpu: cpu}
  end

  # JP Z, a16
  def exec(%Gameboy{} = gb, 0xCA) do
    cpu =
      case CPU.read_flag(gb.cpu, :z) do
        1 ->
          val = Memory.read16(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
          CPU.write_register(gb.cpu, :pc, val)

        0 ->
          CPU.inc_pc(gb.cpu, 3)
      end

    %{gb | cpu: cpu}
  end

  # PREFIX CB
  def exec(%Gameboy{} = gb, 0xCB) do
    val_0 = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc))
    val_1 = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
    exec(gb, val_1 ||| val_0 <<< 8)
  end

  # RL C
  def exec(%Gameboy{} = gb, 0xCB11) do
    carry = CPU.read_flag(gb.cpu, :c)
    c_reg = CPU.read_register(gb.cpu, :c)
    # shift left
    c_reg_shift = (c_reg <<< 1 ||| c_reg >>> (8 - 1)) &&& 0b11111111
    # replace 0th bit with carry,
    c_reg_shift = write_bit(c_reg_shift, 0, carry)
    # update z flag with shifted value
    if nth_bit(c_reg, 7) == 0 do
      CPU.set_flag(gb.cpu, :z)
    else
      CPU.reset_flag(gb.cpu, :z)
    end
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:n)
    |> CPU.write_flag(:c, nth_bit(c_reg, 7))
    |> CPU.write_register(:c, c_reg_shift)
    |> CPU.inc_pc(2)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # SWAP A
  # Z 0 0 0
  def exec(%Gameboy{} = gb, 0xCB37) do
    <<upper::size(4), lower::size(4)>> = <<CPU.read_register(gb.cpu, :a)>>
    <<val::integer>> = <<lower::size(4), upper::size(4)>>

    if val == 0 do
      CPU.set_flag(gb.cpu, :z)
    else
      CPU.reset_flag(gb.cpu, :z)
    end
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:c)
    |> CPU.write_register(:a, val)
    |> CPU.inc_pc(2)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # BIT 4,A
  def exec(%Gameboy{} = gb, 0xCB67) do
    cpu =
      gb.cpu
      |> CPU.inc_pc(2)
      |> (fn cpu ->
            case CPU.test_bit(cpu, :a, 4) do
              0 -> CPU.set_flag(cpu, :z)
              1 -> CPU.reset_flag(cpu, :z)
            end
          end).()
      |> CPU.reset_flag(:n)
      |> CPU.set_flag(:h)

    %{gb | cpu: cpu}
  end

  # BIT 7,H
  def exec(%Gameboy{} = gb, 0xCB7C) do
    CPU.write_register(gb.cpu, :pc, CPU.read_register(gb.cpu, :pc) + 2)
    |> (fn cpu ->
          case CPU.test_bit(cpu, :h, 7) do
            0 -> CPU.set_flag(cpu, :z)
            1 -> CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.set_flag(:h)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # RES 0, A
  def exec(%Gameboy{} = gb, 0xCB87) do
    a = CPU.read_register(gb.cpu, :a) |> reset_bit(0)

    cpu =
      gb.cpu
      |> CPU.write_register(:a, a)
      |> CPU.inc_pc(2)

    %{gb | cpu: cpu}
  end

  # CALL Z, a16
  def exec(%Gameboy{} = gb, 0xCC) do
    case CPU.read_flag(gb.cpu, :z) do
      0 ->
        %{gb | cpu: CPU.write_register(gb.cpu, :pc, CPU.read_register(gb.cpu, :pc) + 3)}

      1 ->
        val = Memory.read16(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
        pc = CPU.read_register(gb.cpu, :pc) + 3
        {pch, pcl} = split_uint16(pc)

        addrh = CPU.read_register(gb.cpu, :sp) - 1
        addrl = CPU.read_register(gb.cpu, :sp) - 2

        memory =
          Memory.write8(gb.memory, addrh, pch)
          |> Memory.write8(addrl, pcl)

        cpu =
          CPU.write_register(gb.cpu, :sp, addrl)
          |> CPU.write_register(:pc, val)

        %{gb | cpu: cpu, memory: memory}
    end
  end

  # CALL a16
  def exec(%Gameboy{} = gb, 0xCD) do
    val = Memory.read16(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
    pc = CPU.read_register(gb.cpu, :pc) + 3
    {pch, pcl} = split_uint16(pc)

    addrh = CPU.read_register(gb.cpu, :sp) - 1
    addrl = CPU.read_register(gb.cpu, :sp) - 2

    memory =
      Memory.write8(gb.memory, addrh, pch)
      |> Memory.write8(addrl, pcl)

    cpu =
      CPU.write_register(gb.cpu, :sp, addrl)
      |> CPU.write_register(:pc, val)

    %{gb | cpu: cpu, memory: memory}
  end

  # ADC A,d8
  # Z 0 H C
  def exec(%Gameboy{} = gb, 0xCE) do
    current = CPU.read_register(gb.cpu, :a)
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :a) + 1)
    [result, <<flags::integer>>] = Arithmetic.addc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc(2)
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # RET NC
  def exec(%Gameboy{} = gb, 0xD0) do
    cpu =
      case CPU.read_flag(gb.cpu, :c) do
        0 ->
          sp = CPU.read_register(gb.cpu, :sp)
          val = Memory.read16(gb.memory, sp)

          gb.cpu
          |> CPU.write_register(:pc, val)
          |> CPU.write_register(:sp, sp + 2)

        1 ->
          CPU.inc_pc(gb.cpu, 1)
      end

    %{gb | cpu: cpu}
  end

  # POP DE
  def exec(%Gameboy{} = gb, 0xD1) do
    sp = CPU.read_register(gb.cpu, :sp)
    data = Memory.read16(gb.memory, sp)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:de, data)
      |> CPU.write_register(:sp, sp + 2)

    %{gb | cpu: cpu}
  end

  # PUSH DE
  @todo "double check this"
  def exec(%Gameboy{} = gb, 0xD5) do
    val = CPU.read_register(gb.cpu, :de)
    {pch, pcl} = split_uint16(val)

    addrh = CPU.read_register(gb.cpu, :sp) - 1
    addrl = CPU.read_register(gb.cpu, :sp) - 2

    memory =
      gb.memory
      |> Memory.write8(addrh, pch)
      |> Memory.write8(addrl, pcl)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:sp, addrl)

    %{gb | cpu: cpu, memory: memory}
  end

  # SUB d8
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0xD6) do
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
    [res, <<flags::integer>>] = Arithmetic.sub(CPU.read_register(gb.cpu, :a), val)

    cpu =
      gb.cpu
      |> CPU.write_register(:a, res)
      |> CPU.write_register(:f, flags)
      |> CPU.inc_pc(2)

    %{gb | cpu: cpu}
  end

  # RETI
  def exec(%Gameboy{} = gb, 0xD9) do
    sp = CPU.read_register(gb.cpu, :sp)
    val = Memory.read16(gb.memory, sp)
    # TODO: enable interrupt as well
    cpu =
      gb.cpu
      |> CPU.write_register(:pc, val)
      |> CPU.write_register(:sp, sp + 2)

    %{gb | cpu: cpu}
  end

  # SBC A,d8
  # Z 1 H C
  def exec(%Gameboy{} = gb, 0xDE) do
    current = CPU.read_register(gb.cpu, :a)
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
    [result, <<flags::integer>>] = Arithmetic.subc(gb.cpu, current, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc(2)
      |> CPU.write_register(:a, result)
      |> CPU.write_register(:f, flags)

    %{gb | cpu: cpu}
  end

  # LDH (a8),A
  def exec(%Gameboy{cpu: %{pc: %{value: 0x2836}}} = gb, 0xE0) do
    pc = CPU.read_register(gb.cpu, :pc) + 2
    {pch, pcl} = split_uint16(pc)

    addrh = CPU.read_register(gb.cpu, :sp) - 1
    addrl = CPU.read_register(gb.cpu, :sp) - 2

    memory =
      Memory.write8(gb.memory, addrh, pch)
      |> Memory.write8(addrl, pcl)

    cpu =
      CPU.write_register(gb.cpu, :sp, addrl)
      |> CPU.write_register(:pc, 0x0040)

    %{gb | cpu: cpu, memory: memory}
  end

  def exec(%Gameboy{} = gb, 0xE0) do
    # if(CPU.read_register(gb.cpu, :pc) == 0x0059) do
    #   IO.puts "scroll y"
    #   IEx.pry
    # end

    addr = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1) + 0xFF00
    memory = Memory.write8(gb.memory, addr, CPU.read_register(gb.cpu, :a))
    cpu = CPU.write_register(gb.cpu, :pc, CPU.read_register(gb.cpu, :pc) + 2)

    %{gb | cpu: cpu, memory: memory}
  end

  # POP HL
  @todo "double check this"
  def exec(%Gameboy{} = gb, 0xE1) do
    sp = CPU.read_register(gb.cpu, :sp)
    data = Memory.read16(gb.memory, sp)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:hl, data)
      |> CPU.write_register(:sp, sp + 2)

    %{gb | cpu: cpu}
  end

  # LD (C),A
  # LD ($FF00+C),A
  # LD dest,src
  def exec(%Gameboy{} = gb, 0xE2) do
    cpu = CPU.inc_pc(gb.cpu, 1)
    addr = CPU.read_register(cpu, :c) + 0xFF00
    memory = Memory.write8(gb.memory, addr, CPU.read_register(cpu, :a))

    %{gb | cpu: cpu, memory: memory}
  end

  # PUSH HL
  def exec(%Gameboy{} = gb, 0xE5) do
    val = CPU.read_register(gb.cpu, :hl)
    {pch, pcl} = split_uint16(val)

    addrh = CPU.read_register(gb.cpu, :sp) - 1
    addrl = CPU.read_register(gb.cpu, :sp) - 2

    memory =
      gb.memory
      |> Memory.write8(addrh, pch)
      |> Memory.write8(addrl, pcl)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:sp, addrl)

    %{gb | cpu: cpu, memory: memory}
  end

  # AND d8
  # Z 0 1 0
  def exec(%Gameboy{} = gb, 0xE6) do
    val =
      gb.memory
      |> Memory.read8(CPU.read_register(gb.cpu, :pc) + 1)
      |> band(CPU.read_register(gb.cpu, :a))
      |> band(0xFF)

    cpu =
      gb.cpu
      |> CPU.inc_pc(2)
      |> CPU.write_register(:a, val)
      |> (fn cpu ->
            if val == 0 do
              CPU.set_flag(cpu, :z)
            else
              CPU.reset_flag(cpu, :z)
            end
          end).()
      |> CPU.reset_flag(:n)
      |> CPU.set_flag(:h)
      |> CPU.reset_flag(:c)

    %{gb | cpu: cpu}
  end

  # ADD SP, r8
  # 0 0 H C
  def exec(%Gameboy{} = gb, 0xE8) do
    current_val = CPU.read_register(gb.cpu, :sp)
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1, :unsigned8)
    [result, <<flags::integer>>] = Arithmetic.add16(current_val, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:hl, result)
      |> CPU.write_register(:f, flags)
      |> CPU.reset_flag(:z)

    %{gb | cpu: cpu}
  end

  # JP HL
  def exec(%Gameboy{} = gb, 0xE9) do
    hl = CPU.read_register(gb.cpu, :hl)
    cpu = CPU.write_register(gb.cpu, :pc, hl)

    %{gb | cpu: cpu}
  end

  # LD (a16),A
  def exec(%Gameboy{} = gb, 0xEA) do
    addr = Memory.read16(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
    memory = Memory.write8(gb.memory, addr, CPU.read_register(gb.cpu, :a))
    cpu = CPU.write_register(gb.cpu, :pc, CPU.read_register(gb.cpu, :pc) + 3)

    %{gb | cpu: cpu, memory: memory}
  end

  # XOR d8
  def exec(%Gameboy{} = gb, 0xEE) do
    data = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
    val = band(0xFF, bxor(CPU.read_register(gb.cpu, :a), data))

    gb.cpu
    |> CPU.inc_pc(2)
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # RST 28H
  def exec(%Gameboy{} = gb, 0xEF) do
    jmp = 0x28

    addrh = CPU.read_register(gb.cpu, :sp) - 1
    addrl = CPU.read_register(gb.cpu, :sp) - 2
    pc = CPU.read_register(gb.cpu, :pc) + 1
    {pch, pcl} = split_uint16(pc)

    memory =
      gb.memory
      |> Memory.write8(addrh, pch)
      |> Memory.write8(addrl, pcl)

    cpu =
      gb.cpu
      |> CPU.write_register(:sp, addrl)
      |> CPU.write_register(:pc, jmp)

    %{gb | memory: memory, cpu: cpu}
  end

  # LDH A,(a8)
  @todo "check"
  def exec(%Gameboy{} = gb, 0xF0) do
    addr = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1) + 0xFF00
    data = Memory.read8(gb.memory, addr)
    # memory = Memory.write8(gb.memory, addr, CPU.read_register(gb.cpu, :a))
    cpu =
      gb.cpu
      |> CPU.write_register(:a, data)
      |> CPU.inc_pc(2)

    %{gb | cpu: cpu}
  end

  # POP AF
  @todo "double check this"
  def exec(%Gameboy{} = gb, 0xF1) do
    sp = CPU.read_register(gb.cpu, :sp)
    data = Memory.read16(gb.memory, sp)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:af, data)
      |> CPU.write_register(:sp, sp + 2)

    %{gb | cpu: cpu}
  end

  # DI
  @todo "implement later"
  def exec(%Gameboy{} = gb, 0xF3) do
    # TODO: actually implement
    %{gb | cpu: CPU.inc_pc(gb.cpu)}
  end

  # PUSH AF
  def exec(%Gameboy{} = gb, 0xF5) do
    val = CPU.read_register(gb.cpu, :af)
    {pch, pcl} = split_uint16(val)

    addrh = CPU.read_register(gb.cpu, :sp) - 1
    addrl = CPU.read_register(gb.cpu, :sp) - 2

    memory =
      gb.memory
      |> Memory.write8(addrh, pch)
      |> Memory.write8(addrl, pcl)

    cpu =
      gb.cpu
      |> CPU.inc_pc()
      |> CPU.write_register(:sp, addrl)

    %{gb | cpu: cpu, memory: memory}
  end

  # OR d8
  # Z 0 0 0
  def exec(%Gameboy{} = gb, 0xF6) do
    data = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
    val = CPU.read_register(gb.cpu, :a) ||| data

    gb.cpu
    |> CPU.inc_pc(2)
    |> CPU.write_register(:a, val)
    |> (fn cpu ->
          if val == 0 do
            CPU.set_flag(cpu, :z)
          else
            CPU.reset_flag(cpu, :z)
          end
        end).()
    |> CPU.reset_flag(:n)
    |> CPU.reset_flag(:h)
    |> CPU.reset_flag(:c)
    |> (fn cpu -> %{gb | cpu: cpu} end).()
  end

  # LD HL,SP + r8
  # 0 0 H C
  def exec(%Gameboy{} = gb, 0xF8) do
    current_val = CPU.read_register(gb.cpu, :hl)
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1, :unsigned8)
    [result, <<flags::integer>>] = Arithmetic.add16(current_val, val)

    cpu =
      gb.cpu
      |> CPU.inc_pc(2)
      |> CPU.write_register(:hl, result)
      |> CPU.write_register(:f, flags)
      |> CPU.reset_flag(:z)
      |> CPU.reset_flag(:n)

    %{gb | cpu: cpu}
  end

  # LD A, (a16)
  def exec(%Gameboy{} = gb, 0xFA) do
    addr = Memory.read16(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)
    data = Memory.read8(gb.memory, addr)

    cpu =
      gb.cpu
      |> CPU.inc_pc(3)
      |> CPU.write_register(:a, data)

    %{gb | cpu: cpu}
  end

  # EI
  @todo "implement"
  def exec(%Gameboy{} = gb, 0xFB) do
    # TODO: actually implement this
    %{gb | cpu: CPU.inc_pc(gb.cpu)}
  end

  # CP d8
  # Z 1 H C
  @todo "fix"
  def exec(%Gameboy{} = gb, 0xFE) do
    val = Memory.read8(gb.memory, CPU.read_register(gb.cpu, :pc) + 1)

    ## temporary hack for Boot ROM and others waiting for vertical blank ##
    # cpu = gb.cpu
    cpu =
      if val >= 0x90 and val <= 0x9A do
        CPU.write_register(gb.cpu, :a, val)
      else
        gb.cpu
      end

    ## temporary hack for Boot ROM and others waiting for vertical blank ##

    a = CPU.read_register(cpu, :a)
    [_res, <<flags::integer>>] = Arithmetic.sub(a, val)

    cpu =
      cpu
      |> CPU.write_register(:f, flags)
      |> CPU.inc_pc(2)

    %{gb | cpu: cpu}
  end

  # RST 38H
  def exec(%Gameboy{} = gb, 0xFF) do
    jmp = 0x38

    addrh = CPU.read_register(gb.cpu, :sp) - 1
    addrl = CPU.read_register(gb.cpu, :sp) - 2
    pc = CPU.read_register(gb.cpu, :pc) + 1
    {pch, pcl} = split_uint16(pc)

    memory =
      gb.memory
      |> Memory.write8(addrh, pch)
      |> Memory.write8(addrl, pcl)

    cpu =
      gb.cpu
      |> CPU.write_register(:sp, addrl)
      |> CPU.write_register(:pc, jmp)

    %{gb | memory: memory, cpu: cpu}
  end

  def exec(%Gameboy{} = gb, opcode) when opcode <= 0xFF do
    IO.puts(format("Unknown opcode: 0x~2.16.0b, #{inspect(gb)}", [opcode]))
    [mnemonic] = Gameboy.Disassembler.disassemble(<<opcode::size(8)>>, [])
    IO.puts("NEXT: #{inspect(mnemonic)}, #{inspect(opcode)}")

    if IEx.started?() do
      IEx.pry()
    end
  end

  def exec(%Gameboy{} = gb, opcode) when opcode > 0xFF do
    IO.puts(format("Unknown opcode: 0x~4.16.0b, #{inspect(gb)}", [opcode]))

    if IEx.started?() do
      IEx.pry()
    end
  end
end
