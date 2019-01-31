defmodule Gameboy.Memory do
  alias Gameboy.Memory
  defstruct data: <<0x00::size(0x10000)-unit(8)>>

  def read8(%Memory{} = mem, %{value: value}) do
    read8(mem, value)
  end

  def read8(%Memory{data: data}, position, type \\ nil) when is_integer(position) do
    case type do
      nil ->
        :binary.at(data, position)

      :uint8 ->
        <<int::little-integer-signed-size(8)>> = <<:binary.at(data, position)>>
        int

      :unsigned8 ->
        <<int::little-integer-unsigned-size(8)>> = <<:binary.at(data, position)>>
        int
    end
  end

  @spec read16(%Gameboy.Memory{}, non_neg_integer()) :: integer()
  def read16(%Memory{data: data}, position) when is_integer(position) do
    <<value::little-size(16)>> = <<:binary.at(data, position), :binary.at(data, position + 1)>>
    value
  end

  @spec write8(%Gameboy.Memory{}, non_neg_integer(), integer()) :: %Gameboy.Memory{}
  def write8(%Memory{data: data}, addr, value) do
    <<first::binary-size(addr), _::binary-size(1), rest::binary>> = data
    %Memory{data: first <> <<value>> <> rest}
  end

  @spec write16(%Gameboy.Memory{}, pos_integer(), integer()) :: %Gameboy.Memory{}
  def write16(%Memory{data: data}, addr, value) do
    first = addr - 1
    <<before::binary-size(first), _old::binary-size(2), rest::binary>> = data
    %Memory{data: before <> <<value::size(16)>> <> rest}
  end

  @spec copy(%Gameboy.Memory{}, binary(), non_neg_integer()) :: %Gameboy.Memory{}
  def copy(%Memory{data: data}, copy_data, position) do
    offset = byte_size(copy_data)
    <<first::binary-size(position), _::binary-size(offset), rest::binary>> = data
    %Memory{data: first <> copy_data <> rest}
  end
end
