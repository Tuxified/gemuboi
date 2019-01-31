defmodule Gameboy.Instructions.Load do
  alias Gameboy.{CPU, Memory}

  @spec ld(Gameboy.CPU.t(), atom(), atom()) :: Gameboy.CPU.t()
  def ld(%Gameboy.CPU{} = cpu, target, source) when is_atom(target) and is_atom(source) do
    val = CPU.read_register(cpu, source)

    cpu
    |> CPU.inc_pc()
    |> CPU.write_register(target, val)
  end

  @spec ld(Gameboy.t(), atom(), atom()) :: Gameboy.t()
  def ld(%Gameboy{} = gb, target, source)
      when is_atom(target) and is_atom(source) and target == :hl do
    cpu = CPU.inc_pc(gb.cpu)
    addr = CPU.read_register(cpu, target)
    memory = Memory.write8(gb.memory, addr, CPU.read_register(gb.cpu, source))

    %{gb | memory: memory, cpu: cpu}
  end
end
