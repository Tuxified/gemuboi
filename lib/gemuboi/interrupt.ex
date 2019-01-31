defmodule Gemuboi.Interrupt do
  @moduledoc """
  Module to handle interrupts, for now only docs

  After each cycle CPU checks for interrupts and executes an interrupt service routine
  an instruction at specific place in mem.

  Interrupts          | ISR address (hex)
  ---------------------------------------
  Vertical blank      | 0040
  LCD status triggers | 0048
  Timer overflow      | 0050
  Serial link         | 0058
  Joypad press        | 0060

  Register            | Location  | Details
  ----------------------------------------------------------------
  Interrupt enable(ie)| FFFF      |
                                | Bit | When 0      | When 1
                                | 0   | Vblank off  | Vblank on
                                | 1   | LCD stat off| LCD stat on
                                | 2   | Timer off   | Timer on
                                | 3   | Serial off  | Serial on
                                | 4   | Joypad off  | Joypad on
  Interrupt flags (if)| FF0F    | When bits are set, an interrupt has happened, bits in the same order as FFFF

  IME = Interrupt master enable, enabled/disabled with EI and DI
  """
end
