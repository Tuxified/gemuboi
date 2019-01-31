defmodule Gameboy.Utils do
  use Bitwise

  def format(fmnt, data) do
    :io_lib.format(fmnt, data) |> to_string
  end

  @todo "checks"
  def nth_bit(number, n) do
    (number &&& 1 <<< n) >>> n
  end

  def write_bit(number, n, 1), do: set_bit(number, n)
  def write_bit(number, n, 0), do: reset_bit(number, n)

  def set_bit(number, n) do
    number ||| 1 <<< n
  end

  def reset_bit(number, n) do
    # (number ||| (0 <<< n))
    band(number, bxor(1 <<< n, 0xFF))
  end

  @todo "check if << upper :: size(8), lower :: size(8) >> works as well"
  def split_uint16(number) do
    # Char1 = MyShort & 0xFF; // lower
    # Char2 = MyShort >> 8; // upper
    {number >>> 8, number &&& 0xFF}
  end
end
