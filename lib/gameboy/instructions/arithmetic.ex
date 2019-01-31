defmodule Gameboy.Instructions.Arithmetic do
  use Bitwise, only: [band: 2]
  alias Gameboy.CPU

  def sub(a, n) do
    # Flags: Z 1 H C
    result = band(0xFF, a - n)
    zero_flag = result == 0x00
    carry_flag = a < n
    half_carry_flag = band(0x0F, a) < band(0x0F, n)

    flags =
      <<flag(zero_flag)::size(1), flag(true)::size(1), flag(half_carry_flag)::size(1),
        flag(carry_flag)::size(1), 0x0::size(4)>>

    [result, flags]
  end

  def add(operand1, operand2) do
    # Flags: Z 0 H C
    result = operand1 + operand2
    masked_result = band(0xFF, result)
    zero_flag = masked_result == 0x00
    carry_flag = result > 0xFF
    half_carry_flag = band(0x0F, operand1) + band(0x0F, operand2) > 0x0F

    flags =
      <<flag(zero_flag)::size(1), flag(false)::size(1), flag(half_carry_flag)::size(1),
        flag(carry_flag)::size(1), 0x0::size(4)>>

    [masked_result, flags]
  end

  # TODO: keep Z flag untouched
  def add16(operand1, operand2) do
    # Flags: - 0 H C
    result = operand1 + operand2
    masked_result = band(0xFFFF, result)
    zero_flag = masked_result == 0x0000
    carry_flag = result > 0xFFFF
    half_carry_flag = band(0x0FFF, operand1) + band(0x0FFF, operand2) > 0x0FFF

    flags =
      <<flag(zero_flag)::size(1), flag(false)::size(1), flag(half_carry_flag)::size(1),
        flag(carry_flag)::size(1), 0x0::size(4)>>

    [masked_result, flags]
  end

  def subc(cpu, current, val) do
    carry = CPU.read_flag(cpu, :c)

    result = current - val - carry
    masked_result = band(0xFF, result)
    zero_flag = masked_result == 0x00
    carry_flag = result < 0x00
    half_carry_flag = band(bxor(bxor(current, val), masked_result), bsl(4, 1)) != 0x00

    flags =
      <<flag(zero_flag)::size(1), flag(true)::size(1), flag(half_carry_flag)::size(1),
        flag(carry_flag)::size(1), 0x0::size(4)>>

    [masked_result, flags]
  end

  def addc(cpu, current, val) do
    carry = CPU.read_flag(cpu, :c)

    result = current + val + carry
    masked_result = band(0xFF, result)
    zero_flag = masked_result == 0x00
    half_carry_flag = band(0x0F, current) + band(0x0F, val) + carry > 0x0F
    carry_flag = result > 0xFF

    flags = <<flag(zero_flag)::size(1), flag(false)::size(1), flag(half_carry_flag)::size(1),
    flag(carry_flag)::size(1), 0x0::size(4)>>

    [masked_result, flags]
  end

  defp flag(true), do: 0b1
  defp flag(false), do: 0b0
end
