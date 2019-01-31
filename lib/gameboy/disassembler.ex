defmodule Gameboy.Disassembler do
  @moduledoc "Disassemble Gameboy code."
  use Gameboy.Opcode

  @doc "Disassemble binary data."
  def disassemble(data, acc \\ [])

  opcode(0x00, "NOOP", 1, 4, [:_, :_, :_, :_])
  opcode(0x01, "LD BC,<%= d16 %>", 3, 12, [:_, :_, :_, :_])
  opcode(0x02, "LD (BC),A", 1, 8, [:_, :_, :_, :_])
  opcode(0x03, "INC BC", 1, 8, [:_, :_, :_, :_])
  opcode(0x04, "INC B", 1, 4, [:z, 0, :h, :_])
  opcode(0x05, "DEC B", 1, 4, [:z, 1, :h, :_])
  opcode(0x06, "LD B, <%= d8 %>", 2, 8, [:_, :_, :_, :_])
  opcode(0x07, "RLCA", 1, 4, [0, 0, 0, :c])
  opcode(0x08, "LD (<%= a16 %>),SP", 3, 20, [:_, :_, :_, :_])
  opcode(0x09, "ADD HL,BC", 1, 8, [:_, 0, :h, :c])
  opcode(0x0A, "LD A,(BC)", 1, 8, [:_, :_, :_, :_])
  opcode(0x0B, "DEC BC", 1, 8, [:_, :_, :_, :_])
  opcode(0x0C, "INC C", 1, 4, [:z, 0, :h, :_])
  opcode(0x0D, "DEC C", 1, 4, [:z, 0, :h, :_])
  opcode(0x0E, "LD C,<%= d8 %>", 2, 8, [:_, :_, :_, :_])
  opcode(0x0F, "RRCA", 1, 4, [0, 0, 0, :c])

  # size: 1 ?
  opcode(0x10, "STOP 0", 2, 4, [:_, :_, :_, :_])
  opcode(0x11, "LD DE,<%= d16 %>", 3, 13, [:_, :_, :_, :_])
  opcode(0x12, "LD (DE),A", 1, 8, [:_, :_, :_, :_])
  opcode(0x13, "INC DE", 1, 8, [:_, :_, :_, :_])
  opcode(0x14, "INC D", 1, 4, [:z, 0, :h, :_])
  opcode(0x15, "DEC D", 1, 4, [:z, 1, :h, :_])
  opcode(0x16, "LD D,<%= d8 %>", 2, 8, [:_, :_, :_, :_])
  opcode(0x17, "RLA", 1, 4, [0, 0, 0, :c])
  opcode(0x18, "JR <%= r8 %>", 2, 12, [:_, :_, :_, :_])
  opcode(0x19, "ADD HL,DE", 1, 8, [:_, 0, :h, :c])
  opcode(0x1A, "LD A,(DE)", 1, 8, [:_, :_, :_, :_])
  opcode(0x1B, "DEC DE", 1, 8, [:_, :_, :_, :_])
  opcode(0x1C, "INC E", 1, 4, [:z, 0, :h, :_])
  opcode(0x1D, "DEC E", 1, 4, [:z, 0, :h, :_])
  opcode(0x1E, "LD E,<%= d8 %>", 2, 8, [:_, :_, :_, :_])
  opcode(0x1F, "RRA", 1, 4, [0, 0, 0, :c])

  opcode(0x20, "JR NZ,<%= r8 %>", 2, {12, 8}, [:_, :_, :_, :_])
  opcode(0x21, "LD HL,<%= d16 %>", 3, 12, [:_, :_, :_, :_])
  opcode(0x22, "LD (HL+),A", 1, 8, [:_, :_, :_, :_])
  opcode(0x23, "INC HL", 1, 8, [:_, :_, :_, :_])
  opcode(0x24, "INC H", 1, 4, [:z, 0, :h, :_])
  opcode(0x25, "DEC H", 1, 4, [:z, 1, :h, :_])
  opcode(0x26, "LD H,<%= d8 %>", 2, 8, [:_, :_, :_, :_])
  opcode(0x27, "DAA", 1, 4, [:z, :_, 0, :c])
  opcode(0x28, "JR Z,<%= r8 %>", 2, {12, 8}, [:_, :_, :_, :_])
  opcode(0x29, "ADD HL,HL", 1, 8, [:_, 0, :h, :c])
  opcode(0x2A, "LD A,(HL+)", 1, 8, [:_, :_, :_, :_])
  opcode(0x2B, "DEC HL", 1, 8, [:_, :_, :_, :_])
  opcode(0x2C, "INC L", 1, 4, [:z, 1, :h, :_])
  opcode(0x2D, "DEC L", 1, 4, [:z, 0, :h, :_])
  opcode(0x2E, "LD L,<%= d8 %>", 2, 8, [:_, :_, :_, :_])
  opcode(0x2F, "CPL", 1, 4, [:_, 1, 1, :_])

  opcode(0x30, "JR NC,<%= r8 %>", 3, {12, 8}, [:_, :_, :_, :_])
  opcode(0x31, "LD SP,<%= d16 %>", 3, 12, [:_, :_, :_, :_])
  opcode(0x32, "LD (HL-),A", 1, 8, [:_, :_, :_, :_])
  opcode(0x33, "INC SP", 1, 8, [:_, :_, :_, :_])
  opcode(0x34, "INC (HL)", 1, 12, [:z, 0, :h, :_])
  opcode(0x35, "DEC (HL)", 1, 12, [:z, 1, :h, :_])
  opcode(0x36, "LD (HL),<%= d8 %>", 1, 12, [:_, :_, :_, :_])
  opcode(0x37, "SCF", 1, 4, [:_, 0, 0, 1])
  opcode(0x38, "JR C,<%= r8 %>", 2, {12, 8}, [:_, :_, :_, :_])
  opcode(0x39, "ADD HL,SP", 1, 8, [:_, 0, :h, :c])
  opcode(0x3A, "LD A,(HL-)", 1, 8, [:_, :_, :_, :_])
  opcode(0x3B, "DEC SP", 1, 8, [:_, :_, :_, :_])
  opcode(0x3C, "INC A", 1, 4, [:z, 0, :h, :_])
  opcode(0x3D, "DEC A", 1, 4, [:z, 1, :h, :_])
  opcode(0x3E, "LD A,<%= d8 %>", 2, 8, [:_, :_, :_, :_])
  opcode(0x3F, "CCF", 2, 4, [:_, 0, 0, :c])

  opcode(0x40, "LD B,B", 1, 4, [:_, :_, :_, :_])
  # opcode 0x41, "", l, c, [z, n, h, c]
  opcode(0x42, "LD B,D", 1, 4, [:_, :_, :_, :_])
  # opcode 0x43, "", l, c, [z, n, h, c]
  # opcode 0x44, "", l, c, [z, n, h, c]
  # opcode 0x45, "", l, c, [z, n, h, c]
  # opcode 0x46, "", l, c, [z, n, h, c]
  # opcode 0x47, "", l, c, [z, n, h, c]
  # opcode 0x48, "", l, c, [z, n, h, c]
  # opcode 0x49, "", l, c, [z, n, h, c]
  # opcode 0x4A, "", l, c, [z, n, h, c]
  # opcode 0x4B, "", l, c, [z, n, h, c]
  # opcode 0x4C, "", l, c, [z, n, h, c]
  # opcode 0x4D, "", l, c, [z, n, h, c]
  # opcode 0x4E, "", l, c, [z, n, h, c]
  opcode(0x4F, "LD C,A", 1, 4, [:_, :_, :_, :_])

  opcode(0x50, "LD D,B", 1, 4, [:_, :_, :_, :_])
  # opcode 0x51, "", l, c, [z, n, h, c]
  # opcode 0x52, "", l, c, [z, n, h, c]
  # opcode 0x53, "", l, c, [z, n, h, c]
  # opcode 0x54, "", l, c, [z, n, h, c]
  # opcode 0x55, "", l, c, [z, n, h, c]
  opcode(0x56, "LD D,(HL)", 1, 8, [:_, :_, :_, :_])
  opcode(0x57, "LD D,A", 1, 4, [:_, :_, :_, :_])
  # opcode 0x58, "", l, c, [z, n, h, c]
  # opcode 0x59, "", l, c, [z, n, h, c]
  # opcode 0x5A, "", l, c, [z, n, h, c]
  # opcode 0x5B, "", l, c, [z, n, h, c]
  # opcode 0x5C, "", l, c, [z, n, h, c]
  # opcode 0x5D, "", l, c, [z, n, h, c]
  opcode(0x5E, "LD E,(HL)", 1, 8, [:_, :_, :_, :_])
  opcode(0x5F, "LD E,A", 1, 4, [:_, :_, :_, :_])

  opcode(0x60, "LD H,B", 1, 4, [:_, :_, :_, :_])
  # opcode 0x61, "", l, c, [z, n, h, c]
  # opcode 0x62, "", l, c, [z, n, h, c]
  # opcode 0x63, "", l, c, [z, n, h, c]
  # opcode 0x64, "", l, c, [z, n, h, c]
  # opcode 0x65, "", l, c, [z, n, h, c]
  opcode(0x66, "LD H,(HL)", 1, 8, [:_, :_, :_, :_])
  opcode(0x67, "LD H,A", 1, 4, [:_, :_, :_, :_])
  opcode(0x68, "LD L, B", 1, 4, [:_, :_, :_, :_])
  # opcode 0x69, "", l, c, [z, n, h, c]
  # opcode 0x6A, "", l, c, [z, n, h, c]
  # opcode 0x6B, "", l, c, [z, n, h, c]
  # opcode 0x6C, "", l, c, [z, n, h, c]
  # opcode 0x6D, "", l, c, [z, n, h, c]
  # opcode 0x6E, "", l, c, [z, n, h, c]
  # opcode 0x6F, "", l, c, [z, n, h, c]

  opcode(0x70, "LD (HL),B", 1, 8, [:_, :_, :_, :_])
  # opcode 0x71, "", l, c, [z, n, h, c]
  # opcode 0x72, "", l, c, [z, n, h, c]
  opcode(0x73, "LD (HL),E", 1, 8, [:_, :_, :_, :_])
  # opcode 0x74, "", l, c, [z, n, h, c]
  # opcode 0x75, "", l, c, [z, n, h, c]
  # opcode 0x76, "", l, c, [z, n, h, c]
  opcode(0x77, "LD (HL),A", 1, 8, [:_, :_, :_, :_])
  opcode(0x78, "LD A,B", 1, 4, [:_, :_, :_, :_])
  # opcode 0x79, "", l, c, [z, n, h, c]
  # opcode 0x7A, "", l, c, [z, n, h, c]
  opcode(0x7B, "LD A,E", 1, 4, [:_, :_, :_, :_])
  opcode(0x7C, "LD A,H", 1, 4, [:_, :_, :_, :_])
  opcode(0x7D, "LD A,L", 1, 4, [:_, :_, :_, :_])
  # opcode 0x7E, "", l, c, [z, n, h, c]
  # opcode 0x7F, "", l, c, [z, n, h, c]

  opcode(0x80, "ADD A,B", 1, 4, [:z, 0, :h, :c])
  opcode(0x81, "ADD A,C", 1, 4, [:z, 0, :h, :c])
  opcode(0x82, "ADD A,D", 1, 4, [:z, 0, :h, :c])
  opcode(0x83, "ADD A,E", 1, 4, [:z, 0, :h, :c])
  opcode(0x84, "ADD A,H", 1, 4, [:z, 0, :h, :c])
  opcode(0x85, "ADD A,L", 1, 4, [:z, 0, :h, :c])
  opcode(0x86, "ADD A,(HL)", 1, 8, [:z, 0, :h, :c])
  opcode(0x87, "ADD A,A", 1, 8, [:z, 0, :h, :c])
  opcode(0x88, "ADC A,B", 1, 4, [:z, 0, :h, :c])
  opcode(0x89, "ADC A,C", 1, 4, [:z, 0, :h, :c])
  opcode(0x8A, "ADC A,D", 1, 4, [:z, 0, :h, :c])
  opcode(0x8B, "ADC A,E", 1, 4, [:z, 0, :h, :c])
  opcode(0x8C, "ADC A,H", 1, 4, [:z, 0, :h, :c])
  opcode(0x8D, "ADC A,L", 1, 4, [:z, 0, :h, :c])
  opcode(0x8E, "ADC A,(HL)", 1, 8, [:z, 0, :h, :c])
  opcode(0x8F, "ADC A,A", 1, 4, [:z, 0, :h, :c])

  opcode(0x90, "SUB B", 1, 4, [:z, 1, :h, :c])
  # opcode 0x91, "", l, c, [z, n, h, c]
  # opcode 0x92, "", l, c, [z, n, h, c]
  # opcode 0x93, "", l, c, [z, n, h, c]
  # opcode 0x94, "", l, c, [z, n, h, c]
  # opcode 0x95, "", l, c, [z, n, h, c]
  # opcode 0x96, "", l, c, [z, n, h, c]
  # opcode 0x97, "", l, c, [z, n, h, c]
  # opcode 0x98, "", l, c, [z, n, h, c]
  # opcode 0x99, "", l, c, [z, n, h, c]
  # opcode 0x9A, "", l, c, [z, n, h, c]
  # opcode 0x9B, "", l, c, [z, n, h, c]
  # opcode 0x9C, "", l, c, [z, n, h, c]
  # opcode 0x9D, "", l, c, [z, n, h, c]
  # opcode 0x9E, "", l, c, [z, n, h, c]
  # opcode 0x9F, "", l, c, [z, n, h, c]

  opcode(0xA0, "AND B", 1, 4, [:z, 0, 1, 0])
  opcode(0xA1, "AND C", 1, 4, [:z, 0, 1, 0])
  # opcode 0xA2, "", l, c, [z, n, h, c]
  # opcode 0xA3, "", l, c, [z, n, h, c]
  # opcode 0xA4, "", l, c, [z, n, h, c]
  # opcode 0xA5, "", l, c, [z, n, h, c]
  # opcode 0xA6, "", l, c, [z, n, h, c]
  # opcode 0xA7, "", l, c, [z, n, h, c]
  # opcode 0xA8, "", l, c, [z, n, h, c]
  # opcode 0xA9, "", l, c, [z, n, h, c]
  # opcode 0xAA, "", l, c, [z, n, h, c]
  # opcode 0xAB, "", l, c, [z, n, h, c]
  # opcode 0xAC, "", l, c, [z, n, h, c]
  # opcode 0xAD, "", l, c, [z, n, h, c]
  # opcode 0xAE, "", l, c, [z, n, h, c]
  opcode(0xAF, "XOR A", 1, 4, [:z, 0, 0, 0])

  opcode(0xB0, "OR B", 1, 4, [:z, 0, 0, 0])
  opcode(0xB1, "OR C", 1, 4, [:z, 0, 0, 0])
  opcode(0xB2, "OR D", 1, 4, [:z, 0, 0, 0])
  opcode(0xB3, "OR E", 1, 4, [:z, 0, 0, 0])
  opcode(0xB4, "OR H", 1, 4, [:z, 0, 0, 0])
  opcode(0xB5, "OR L", 1, 4, [:z, 0, 0, 0])
  opcode(0xB6, "OR (HL)", 1, 4, [:z, 0, 0, 0])
  opcode(0xB7, "OR A", 1, 4, [:z, 0, 0, 0])
  opcode(0xB8, "CP A", 1, 4, [:z, 1, :h, :c])
  opcode(0xB9, "CP C", 1, 4, [:z, 1, :h, :c])
  opcode(0xBA, "CP D", 1, 4, [:z, 1, :h, :c])
  opcode(0xBB, "CP E", 1, 4, [:z, 1, :h, :c])
  opcode(0xBC, "CP H", 1, 4, [:z, 1, :h, :c])
  opcode(0xBD, "CP L", 1, 4, [:z, 1, :h, :c])
  opcode(0xBE, "CP (HL)", 1, 8, [:z, 1, :h, :c])
  opcode(0xBF, "CP A", 1, 4, [:z, 1, :h, :c])

  opcode(0xC0, "RET NZ", 1, {20, 8}, [:_, :_, :_, :_])
  opcode(0xC1, "POP BC", 1, 12, [:_, :_, :_, :_])
  opcode(0xC2, "JP NZ <%= a16 %>", 3, {16, 12}, [:_, :_, :_, :_])
  opcode(0xC3, "JP <%= a16 %>", 3, 16, [:_, :_, :_, :_])
  opcode(0xC4, "CALL NZ, <%= a16 %>", 3, {24, 12}, [:_, :_, :_, :_])
  opcode(0xC5, "PUSH BC", 1, 16, [:_, :_, :_, :_])
  # opcode 0xC6, "", l, c, [z, n, h, c]
  # opcode 0xC7, "", l, c, [z, n, h, c]
  opcode(0xC8, "RET Z", 1, {20, 8}, [:_, :_, :_, :_])
  opcode(0xC9, "RET", 1, 16, [:_, :_, :_, :_])
  # opcode 0xCA, "", l, c, [z, n, h, c]

  opcode(0xCB11, "RL C", 2, 8, [:z, 0, 0, :c])
  opcode(0xCB7C, "BIT 7,H", 2, 8, [:z, 0, 1, :_])
  opcode(0xCB87, "RES 0, A", 2, 8, [:_, :_, :_, :_])

  opcode(0xCA, "JP Z,<%= a16 %>", 3, {16, 12}, [:_, :_, :_, :_])
  opcode(0xCC, "CALL Z,<%= a16 %>", 3, {24, 12}, [:_, :_, :_, :_])
  opcode(0xCD, "CALL <%= a16 %>", 3, 24, [:_, :_, :_, :_])
  opcode(0xCE, "ADC A, <%= d8 %>", 2, 8, [:z, 0, :h, :c])
  # opcode 0xCF, "", l, c, [z, n, h, c]

  opcode(0xD0, "RET NC", 1, {20, 8}, [:_, :_, :_, :_])
  opcode(0xD1, "POP DE", 1, 12, [:_, :_, :_, :_])
  opcode(0xD2, "JP NC, <%= a16 %>", 3, {16, 12}, [:_, :_, :_, :_])
  opcode(0xD4, "CALL NC,<%= a16 %>", 3, {24, 12}, [:_, :_, :_, :_])
  opcode(0xD5, "PUSH DE", 1, 16, [:_, :_, :_, :_])
  opcode(0xD6, "SUB <%= d8 %>", 2, 8, [:z, 1, :h, :c])
  opcode(0xD7, "RST 10H", 1, 16, [:_, :_, :_, :_])
  opcode(0xD8, "REC C", 1, {20, 8}, [:_, :_, :_, :_])
  opcode(0xD9, "RETI", 1, 16, [:_, :_, :_, :_])
  opcode(0xDA, "JP C,<%= a16 %>", 3, {16, 12}, [:_, :_, :_, :_])
  opcode(0xDC, "CALL C,<%= a16 %>", 3, {24, 12}, [:_, :_, :_, :_])
  opcode(0xDE, "SBC A,<%= d8 %>", 2, 8, [:z, 1, :h, :c])
  opcode(0xDF, "RST 18H", 1, 16, [:_, :_, :_, :_])

  opcode(0xE0, "LDH (<%= a8 %>),A", 2, 12, [:_, :_, :_, :_])
  opcode(0xE1, "POP HL", 1, 12, [:_, :_, :_, :_])
  # fixed: https://github.com/lmmendes/game-boy-opcodes/pull/1
  opcode(0xE2, "LD ($FF00+C),A", 1, 8, [:_, :_, :_, :_])
  opcode(0xE5, "PUSH HL", 1, 16, [:_, :_, :_, :_])
  opcode(0xE6, "AND <%= d8 %>", 2, 8, [:z, 0, 1, 0])
  opcode(0xE7, "RST 20H", 1, 16, [:_, :_, :_, :_])
  opcode(0xE8, "AND SP,<%= r8 %>", 2, 16, [0, 0, :h, :c])
  opcode(0xE9, "JP (HL)", 1, 4, [:_, :_, :_, :_])
  opcode(0xEA, "LD (<%= a16 %>),A", 3, 16, [:_, :_, :_, :_])
  opcode(0xEE, "XOR <%= d8 %>", 2, 8, [:z, 0, 0, 0])
  opcode(0xEF, "RST 28H", 1, 16, [:_, :_, :_, :_])

  opcode(0xF0, "LDH A,(<%= a8 %>)", 2, 12, [:_, :_, :_, :_])
  opcode(0xF1, "POP AF", 1, 12, [:z, :n, :h, :c])
  # fixed: https://github.com/lmmendes/game-boy-opcodes/pull/1
  opcode(0xF2, "LD A,(C)", 1, 8, [:_, :_, :_, :_])
  opcode(0xF3, "DI", 1, 4, [:_, :_, :_, :_])
  opcode(0xF5, "PUSH AF", 1, 16, [:_, :_, :_, :_])
  opcode(0xF6, "OR <%= d8 %>", 2, 8, [:z, 0, 0, 0])
  opcode(0xF7, "RST 30H", 1, 16, [:_, :_, :_, :_])
  opcode(0xF8, "LD HL,SP+<%= r8 %>", 2, 12, [0, 0, :h, :c])
  opcode(0xF9, "LD SP,HL", 1, 8, [:_, :_, :_, :_])
  opcode(0xFA, "LD A,(<%= a16 %>)", 3, 16, [:_, :_, :_, :_])
  opcode(0xFB, "EI", 1, 4, [:_, :_, :_, :_])
  opcode(0xFE, "CP <%= d8 %>", 2, 8, [:z, 1, :h, :c])
  opcode(0xFF, "RST 38H", 1, 16, [:_, :_, :_, :_])

  def disassemble(<<0xCB, code::integer-size(8), _::binary>>, _acc) do
    msg = :io_lib.format("Unknown CB Prefix opcode: 0x~2.16.0b", [code]) |> to_string
    raise(msg)
  end

  def disassemble(<<code::integer-size(8), rest::binary>>, acc) do
    msg = :io_lib.format("$~2.16.0b", [code]) |> to_string
    disassemble(rest, [msg | acc])
  end

  def disassemble(<<>>, acc), do: Enum.reverse(acc)

  def lookup(op) do
    %Gameboy.Opcode{
      opcode: op,
      mnemonic: "",
      byte_length: 1,
      cycles: 0,
      value: nil,
      flags: []
    }
  end
end
