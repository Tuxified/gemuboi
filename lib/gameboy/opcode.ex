defmodule Gameboy.Opcode do
  @moduledoc """
  DSL for defining an opcode.

  LD A,(C) has alternative mnemonic LD A,($FF00+C)
  LD C,(A) has alternative mnemonic LD ($FF00+C),A
  LDH A,(a8) has alternative mnemonic LD A,($FF00+a8)
  LDH (a8),A has alternative mnemonic LD ($FF00+a8),A
  LD A,(HL+) has alternative mnemonic LD A,(HLI) or LDI A,(HL)
  LD (HL+),A has alternative mnemonic LD (HLI),A or LDI (HL),A
  LD A,(HL-) has alternative mnemonic LD A,(HLD) or LDD A,(HL)
  LD (HL-),A has alternative mnemonic LD (HLD),A or LDD (HL),A
  LD HL,SP+r8 has alternative mnemonic LDHL SP,r8

  Source: https://max-m.github.io/gb-docs//optables/classic
  """

  defstruct [
    :opcode,
    :mnemonic,
    :byte_length,
    :cycles,
    :value,
    :flags
  ]

  defimpl String.Chars, for: Gameboy.Opcode do
    def to_string(%{mnemonic: mnemonic}) do
      mnemonic
    end
  end

  @doc false
  defmacro __using__(_) do
    quote do
      import Gameboy.Opcode, only: [opcode: 5]
    end
  end

  @doc """
  Define an opcode.
  * `op` - Number code.
  * `mnemonic` - template for human readable mnemonic. May contain several
  `<%= %>` tags.
    * `<%= d8 %>` - immediate 8 bit data
    * `<%= d16 %>` - immediate 16 bit data
    * `<%= a8 %>` - 8 bit unsigned data, which are added to $FF00 in certain instructions (replacement for missing IN and OUT instructions)
    * `<%= a16 %>` - 16 bit address
    * `<%= r8 %>` - 8 bit signed data, which are added to program counter
  * `byte_length` - defines which of the above tags will be available, and how long
  opcode is.
  * `cycles` - number of cpu cycles an opcode will take.
  * `flags` - Flags that will be modified by this opcode.
    * `:z` - Zero Flag
    * `:n` - Subtract Flag
    * `:h` - Half Carry Flag
    * `:c` - Carry Flag
    * `:_` - No effect.

  ## Examples

      opcode 0x0E, "LD C,<%= d8 %>", 2, 8, [:_, :_, :_, :_]
      opcode 0x0D, "DEC C", 1, 4, [:z, 0, :h, :_]

  """
  defmacro opcode(op, mnemonic, byte_length, cycles, flags_altered)

  defmacro opcode(op, mnemonic, byte_length = 1, cycles, flags_altered) do
    quote do
      def disassemble(<<unquote(op), rest::binary>>, acc) do
        mnemonic = unquote(mnemonic)

        dis =
          struct(
            Gameboy.Opcode,
            opcode: unquote(op),
            mnemonic: mnemonic,
            byte_length: unquote(byte_length),
            cycles: unquote(cycles),
            value: nil,
            flags: unquote(flags_altered)
          )

        disassemble(rest, [dis | acc])
      end

      def lookup(unquote(op)) do
        struct(
          Gameboy.Opcode,
          opcode: unquote(op),
          mnemonic: unquote(mnemonic),
          byte_length: unquote(byte_length),
          cycles: unquote(cycles),
          value: nil,
          flags: unquote(flags_altered)
        )
      end
    end
  end

  defmacro opcode(op, mnemonic, byte_length = 2, cycles, flags_altered) when op > 0xFF do
    quote do
      def disassemble(<<unquote(op)::integer-size(16), rest::binary>> = bin, acc) do
        mnemonic = EEx.eval_string(unquote(mnemonic))

        dis =
          struct(
            Gameboy.Opcode,
            opcode: unquote(op),
            mnemonic: mnemonic,
            byte_length: unquote(byte_length),
            cycles: unquote(cycles),
            value: nil,
            flags: unquote(flags_altered)
          )

        disassemble(rest, [dis | acc])
      end

      def lookup(unquote(op)) do
        struct(
          Gameboy.Opcode,
          opcode: unquote(op),
          mnemonic: unquote(mnemonic),
          byte_length: unquote(byte_length),
          cycles: unquote(cycles),
          value: nil,
          flags: unquote(flags_altered)
        )
      end
    end
  end

  defmacro opcode(op, mnemonic, byte_length = 2, cycles, flags_altered) do
    quote do
      def disassemble(<<unquote(op)::integer-size(8), arg1::size(8), rest::binary>>, acc) do
        arg1 = :io_lib.format('$~2.16.0b', [arg1])
        mnemonic = EEx.eval_string(unquote(mnemonic), r8: arg1, d8: arg1, a8: arg1)

        dis =
          struct(
            Gameboy.Opcode,
            opcode: unquote(op),
            mnemonic: mnemonic,
            byte_length: unquote(byte_length),
            cycles: unquote(cycles),
            value: arg1,
            flags: unquote(flags_altered)
          )

        disassemble(rest, [dis | acc])
      end

      def lookup(unquote(op)) do
        struct(
          Gameboy.Opcode,
          opcode: unquote(op),
          mnemonic: unquote(mnemonic),
          byte_length: unquote(byte_length),
          cycles: unquote(cycles),
          value: nil,
          flags: unquote(flags_altered)
        )
      end
    end
  end

  defmacro opcode(op, mnemonic, byte_length = 3, cycles, flags_altered) do
    quote do
      def disassemble(<<unquote(op)::integer-size(8), arg1::little-size(16), r::binary>>, acc) do
        arg1 = :io_lib.format('$~4.16.0b', [arg1])
        mnemonic = EEx.eval_string(unquote(mnemonic), d16: arg1, a16: arg1)

        dis =
          struct(
            Gameboy.Opcode,
            opcode: unquote(op),
            mnemonic: mnemonic,
            byte_length: unquote(byte_length),
            cycles: unquote(cycles),
            value: arg1,
            flags: unquote(flags_altered)
          )

        disassemble(r, [dis | acc])
      end

      def lookup(unquote(op)) do
        struct(
          Gameboy.Opcode,
          opcode: unquote(op),
          mnemonic: unquote(mnemonic),
          byte_length: unquote(byte_length),
          cycles: unquote(cycles),
          value: nil,
          flags: unquote(flags_altered)
        )
      end
    end
  end
end
