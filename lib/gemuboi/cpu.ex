defmodule Gemuboi.Cpu do
  defstruct [:register, :m, :ime, :halt, :di_pending, :ie_pending]

  defmodule Register do
    @moduledoc """
    Module for CPU register. A is an accumulator, F holds flags. SP is stack point, PC program counter
    each hold 8bits of data, but can be combined with another in 16bits.
    AF, BC, DE, HL
    """
    defstruct a: 0x00, b: 0x00, c: 0x00, d: 0x00, e: 0x00, f: 0x00, sp: 0x0000, pc: 0x0000

    defmodule Flags do
      defstruct zero: false, subtract: false, half_carry: false, carry: false

      def extract(
            <<zero::size(1), subtract::size(1), half_carry::size(1), carry::size(1),
              _rest::size(4)>>
          ) do
        %__MODULE__{
          zero: to_bool(zero),
          subtract: to_bool(subtract),
          half_carry: to_bool(half_carry),
          carry: to_bool(carry)
        }
      end

      def set(), do: :TODO

      defp to_bool(<<1::size(1)>>), do: true
      defp to_bool(<<0::size(1)>>), do: false
    end
  end

  defmodule Instructions do
    # http://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
    # Opcodes:
    # LD - load
    # add, sub
    # and, xor, or
    # RLA - rotate bit
    # DEC - decrement value in register
    # PUSH - something on stack
    # CALL - call subroutine at address
    # NOP - noop
    # POP - pop from stack
    # HALT
    # EI -
  end

  defmodule MMU do
    @memory_size 65_536
    def init(_args \\ []) do
      :array.new(size: @memory_size, fixed: true, default: 0)
    end

    def read(mmu, ind) do
      case ind do
        # ROM (0 + n?)
        0x0000..0x8000 ->
          # read from cartridge
          :noop

        # video ROM
        0x8000..0xA000 ->
          :array.get(ind, mmu)

        #  RAM Bank
        0xA000..0xC000 ->
          # read from cartridge
          :noop

        # User RAM / IO / Sprites / Stack
        0xC000..0xFFFF ->
          :array.get(ind, mmu)
      end
    end
  end

  def init() do
    %Gemuboi.Cpu{register: %Gemuboi.Cpu.Register{}, ime: 0, halt: false, m: 0}
  end
end
