defmodule Gameboy.CPU do
  use Bitwise
  alias Gameboy.CPU
  import Gameboy.Utils
  require Logger

  defmodule Register do
    defstruct [:name, :value]

    defimpl Inspect, for: Register do
      def inspect(data, _) do
        Gameboy.Utils.format("#Reg<0x~4.16.0b>", [data.value])
      end
    end
  end

  defstruct af: struct(Register, name: :af, value: 0x00),
            bc: struct(Register, name: :bc, value: 0x00),
            de: struct(Register, name: :de, value: 0x00),
            hl: struct(Register, name: :hl, value: 0x00),
            sp: struct(Register, name: :sp, value: 0x00),
            pc: struct(Register, name: :pc, value: 0x00)

  def test_bit(%CPU{} = cpu, reg, bit) do
    reg_val = read_register(cpu, reg)
    (reg_val &&& 1 <<< bit) >>> bit
  end

  @doc "DEC n register only affects flags for 8 bit registers"
  def dec_register(%CPU{} = cpu, reg) when reg in [:bc, :de, :hl, :sp] do
    current_val = read_register(cpu, reg)
    write_register(cpu, reg, current_val - 1)
  end

  def dec_register(%CPU{} = cpu, reg) do
    current_val = read_register(cpu, reg)
    new_val = band(0xFF, current_val - 1)

    cpu =
      write_register(cpu, reg, new_val)
      |> set_flag(:n)

    cpu = if new_val == 0, do: set_flag(cpu, :z), else: reset_flag(cpu, :z)
    # cpu = if nth_bit(new_val, 4) == 1, do: set_flag(cpu, :h), else: reset_flag(cpu, :h)
    if band(0x0F, current_val) == 0x00, do: set_flag(cpu, :h), else: reset_flag(cpu, :h)
  end

  def inc_pc(cpu, steps \\ 1)

  def inc_pc(%CPU{} = cpu, steps) do
    current_val = read_register(cpu, :pc)
    write_register(cpu, :pc, current_val + steps)
  end

  @doc "INC n register only affects flags for 8 bit registers"
  def inc_register(%CPU{} = cpu, reg) when reg in [:bc, :de, :hl, :sp] do
    current_val = read_register(cpu, reg)
    write_register(cpu, reg, current_val + 1)
  end

  def inc_register(%CPU{} = cpu, reg) do
    current_val = read_register(cpu, reg)
    new_val = band(0xFF, current_val + 1)
    halfCarryFlag = band(0x0F, current_val) + 1 > 0x0F

    cpu =
      write_register(cpu, reg, new_val)
      |> reset_flag(:n)

    cpu = if new_val == 0, do: set_flag(cpu, :z), else: reset_flag(cpu, :z)
    if halfCarryFlag, do: set_flag(cpu, :h), else: reset_flag(cpu, :h)
  end

  def write_register(%CPU{} = cpu, reg, val) when val > 0xFFFF do
    difference = val - 0xFFFF
    Logger.warn("Register [#{reg}] wrapping: #{inspect(val, base: :hex)}")
    Logger.warn("Current SP #{inspect(cpu.sp)}")
    Logger.warn("Currently at #{inspect(cpu.pc)}")
    write_register(cpu, reg, 0 + difference)
  end

  def write_register(%CPU{} = cpu, :pc, val), do: %{cpu | pc: %{cpu.pc | value: val}}
  def write_register(%CPU{} = cpu, :sp, val), do: %{cpu | sp: %{cpu.sp | value: val}}
  def write_register(%CPU{} = cpu, :af, val), do: %{cpu | af: %{cpu.af | value: val}}
  def write_register(%CPU{} = cpu, :bc, val), do: %{cpu | bc: %{cpu.bc | value: val}}
  def write_register(%CPU{} = cpu, :de, val), do: %{cpu | de: %{cpu.de | value: val}}
  def write_register(%CPU{} = cpu, :hl, val), do: %{cpu | hl: %{cpu.hl | value: val}}

  def write_register(%CPU{} = cpu, :a, val) when val <= 0xFF do
    new_val = (cpu.af.value &&& ~~~(0xFF <<< (1 * 8))) ||| val <<< (1 * 8)
    %CPU{cpu | af: %{cpu.af | value: new_val}}
  end

  def write_register(%CPU{} = cpu, :f, val) when val <= 0xFF do
    new_val = (cpu.af.value &&& ~~~(0xFF <<< (0 * 8))) ||| val <<< (0 * 8)
    %CPU{cpu | af: %{cpu.af | value: new_val}}
  end

  def write_register(%CPU{} = cpu, :h, val) when val <= 0xFF do
    new_val = (cpu.hl.value &&& ~~~(0xFF <<< (1 * 8))) ||| val <<< (1 * 8)
    %CPU{cpu | hl: %{cpu.hl | value: new_val}}
  end

  def write_register(%CPU{} = cpu, :l, val) when val <= 0xFF do
    new_val = (cpu.hl.value &&& ~~~(0xFF <<< (0 * 8))) ||| val <<< (0 * 8)
    %CPU{cpu | hl: %{cpu.hl | value: new_val}}
  end

  def write_register(%CPU{} = cpu, :b, val) when val <= 0xFF do
    new_val = (cpu.bc.value &&& ~~~(0xFF <<< (1 * 8))) ||| val <<< (1 * 8)
    %CPU{cpu | bc: %{cpu.bc | value: new_val}}
  end

  def write_register(%CPU{} = cpu, :c, val) when val <= 0xFF do
    new_val = (cpu.bc.value &&& ~~~(0xFF <<< (0 * 8))) ||| val <<< (0 * 8)
    %CPU{cpu | bc: %{cpu.bc | value: new_val}}
  end

  def write_register(%CPU{} = cpu, :d, val) when val <= 0xFF do
    new_val = (cpu.de.value &&& ~~~(0xFF <<< (1 * 8))) ||| val <<< (1 * 8)
    %CPU{cpu | de: %{cpu.de | value: new_val}}
  end

  def write_register(%CPU{} = cpu, :e, val) when val <= 0xFF do
    new_val = (cpu.de.value &&& ~~~(0xFF <<< (0 * 8))) ||| val <<< (0 * 8)
    %CPU{cpu | de: %{cpu.de | value: new_val}}
  end

  def read_register(%CPU{} = cpu, :af), do: cpu.af.value
  def read_register(%CPU{} = cpu, :bc), do: cpu.bc.value
  def read_register(%CPU{} = cpu, :de), do: cpu.de.value
  def read_register(%CPU{} = cpu, :hl), do: cpu.hl.value
  def read_register(%CPU{} = cpu, :sp), do: cpu.sp.value
  def read_register(%CPU{} = cpu, :pc), do: cpu.pc.value

  def read_register(%CPU{} = cpu, :a) do
    cpu.af.value >>> (8 * 1) &&& 0xFF
  end

  def read_register(%CPU{} = cpu, :f) do
    cpu.af.value >>> (8 * 0) &&& 0xFF
  end

  def read_register(%CPU{} = cpu, :h) do
    cpu.hl.value >>> (8 * 1) &&& 0xFF
  end

  def read_register(%CPU{} = cpu, :l) do
    cpu.hl.value >>> (8 * 0) &&& 0xFF
  end

  def read_register(%CPU{} = cpu, :d) do
    cpu.de.value >>> (8 * 1) &&& 0xFF
  end

  def read_register(%CPU{} = cpu, :e) do
    cpu.de.value >>> (8 * 0) &&& 0xFF
  end

  def read_register(%CPU{} = cpu, :b) do
    cpu.bc.value >>> (8 * 1) &&& 0xFF
  end

  def read_register(%CPU{} = cpu, :c) do
    cpu.bc.value >>> (8 * 0) &&& 0xFF
  end

  def set_flag(%CPU{} = cpu, :z) do
    current = read_register(cpu, :f)
    write_register(cpu, :f, set_bit(current, 7))
  end

  def set_flag(%CPU{} = cpu, :n) do
    current = read_register(cpu, :f)
    write_register(cpu, :f, set_bit(current, 6))
  end

  def set_flag(%CPU{} = cpu, :h) do
    current = read_register(cpu, :f)
    write_register(cpu, :f, set_bit(current, 5))
  end

  def set_flag(%CPU{} = cpu, :c) do
    current = read_register(cpu, :f)
    write_register(cpu, :f, set_bit(current, 4))
  end

  def reset_flag(%CPU{} = cpu, :z) do
    current = read_register(cpu, :f)
    write_register(cpu, :f, reset_bit(current, 7))
  end

  def reset_flag(%CPU{} = cpu, :n) do
    current = read_register(cpu, :f)
    write_register(cpu, :f, reset_bit(current, 6))
  end

  def reset_flag(%CPU{} = cpu, :h) do
    current = read_register(cpu, :f)
    write_register(cpu, :f, reset_bit(current, 5))
  end

  def reset_flag(%CPU{} = cpu, :c) do
    current = read_register(cpu, :f)
    write_register(cpu, :f, reset_bit(current, 4))
  end

  def read_flag(%CPU{} = cpu, :z) do
    current = read_register(cpu, :f)
    nth_bit(current, 7)
  end

  def read_flag(%CPU{} = cpu, :n) do
    current = read_register(cpu, :f)
    nth_bit(current, 6)
  end

  def read_flag(%CPU{} = cpu, :h) do
    current = read_register(cpu, :f)
    nth_bit(current, 5)
  end

  def read_flag(%CPU{} = cpu, :c) do
    current = read_register(cpu, :f)
    nth_bit(current, 4)
  end

  def write_flag(%CPU{} = cpu, flag, 1), do: set_flag(cpu, flag)
  def write_flag(%CPU{} = cpu, flag, 0), do: reset_flag(cpu, flag)
end
