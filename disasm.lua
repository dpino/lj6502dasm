#!/usr/bin/env luajit

local ffi = require("ffi")
local bit = require("bit")

-- Addressing modes.
local IMMED = 0    -- Immediate.
local ABSOL = 1    -- Absolute.
local ZEROP = 2    -- Zero Page.
local IMPLI = 3    -- Implied.
local INDIA = 4    -- Indirect Absolute.
local ABSIX = 5    -- Absolute indexed with X.
local ABSIY = 6    -- Absolute indexed with Y.
local ZEPIX = 7    -- Zero page indexed with X.
local ZEPIY = 8    -- Zero page indexed with Y.
local INDIN = 9    -- Indexed indirect (with X).
local ININD = 10   -- Indirect indexed (with Y).
local RELAT = 11   -- Relative.
local ACCUM = 12   -- Accumulator.

local bor = bit.bor

local pc = 0
local base = 0x8000

local opcode_t = ffi.typeof[[
   struct {
      const char *name;            // Opcode name.
      uint8_t addr_mode;           // Addressing mode.
   } 
]]
                
local function opc (t)
   assert(type(t) == "table")
   return ffi.new(opcode_t, t)
end             

-- List of opcodes.
local opcode_table = {
   -- ADC.
   [0x69] = opc{"ADC", IMMED},
   [0x65] = opc{"ADC", ZEROP},
   [0x75] = opc{"ADC", ZEPIX},
   [0x6D] = opc{"ADC", ABSOL},
   [0x7D] = opc{"ADC", ABSIX},
   [0x79] = opc{"ADC", ABSIY},
   [0x61] = opc{"ADC", INDIN},
   [0x71] = opc{"ADC", ININD},
   -- AND.
   [0x29] = opc{"AND", IMMED},
   [0x25] = opc{"AND", ZEROP},
   [0x35] = opc{"AND", ZEPIX},
   [0x2D] = opc{"AND", ABSOL},
   [0x3D] = opc{"AND", ABSIX},
   [0x39] = opc{"AND", ABSIY},
   [0x21] = opc{"AND", INDIN},
   [0x31] = opc{"AND", ININD},
   -- ASL.
   [0x0A] = opc{"ASL", ACCUM},
   [0x06] = opc{"ASL", ZEROP},
   [0x16] = opc{"ASL", ZEPIX},
   [0x0E] = opc{"ASL", ABSOL},
   [0x1E] = opc{"ASL", ABSIX},
   -- BCC 
   [0x90] = opc{"BCC", RELAT},
   -- BCS 
   [0xB0] = opc{"BCS", RELAT},
   -- BEQ 
   [0xF0] = opc{"BEQ", RELAT},
    -- BIT 
   [0x24] = opc{"BIT", ZEROP},
   [0x2C] = opc{"BIT", ABSOL},
   -- BMI 
   [0x30] = opc{"BMI", RELAT},
   -- BNE 
   [0xD0] = opc{"BNE", RELAT},
   -- BPL 
   [0x10] = opc{"BPL", RELAT},
   -- BRK 
   [0x00] = opc{"BRK", IMPLI},
   -- BVC 
   [0x50] = opc{"BVC", RELAT},
   -- BVS 
   [0x70] = opc{"BVS", RELAT},
   -- CLC 
   [0x18] = opc{"CLC", IMPLI}, 
   -- CLD 
   [0xD8] = opc{"CLD", IMPLI}, 
   -- CLI 
   [0x58] = opc{"CLI", IMPLI}, 
   -- CLV 
   [0xB8] = opc{"CLV", IMPLI}, 
   -- CMP 
   [0xC9] = opc{"CMP", IMMED}, 
   [0xC5] = opc{"CMP", ZEROP},
   [0xD5] = opc{"CMP", ZEPIX},
   [0xCD] = opc{"CMP", ABSOL},
   [0xDD] = opc{"CMP", ABSIX},
   [0xD9] = opc{"CMP", ABSIY},
   [0xC1] = opc{"CMP", INDIN},
   [0xD1] = opc{"CMP", ININD},
   -- CPX 
   [0xE0] = opc{"CPX", IMMED}, 
   [0xE4] = opc{"CPX", ZEROP},
   [0xEC] = opc{"CPX", ABSOL},
   -- CPY 
   [0xC0] = opc{"CPY", IMMED}, 
   [0xC4] = opc{"CPY", ZEROP},
   [0xCC] = opc{"CPY", ABSOL},
   -- DEC 
   [0xC6] = opc{"DEC", ZEROP}, 
   [0xD6] = opc{"DEC", ZEPIX},
   [0xCE] = opc{"DEC", ABSOL},
   [0xDE] = opc{"DEC", ABSIX},
   -- DEX 
   [0xCA] = opc{"DEX", IMPLI}, 
   -- DEY 
   [0x88] = opc{"DEY", IMPLI}, 
   -- EOR 
   [0x49] = opc{"EOR", IMMED}, 
   [0x45] = opc{"EOR", ZEROP},
   [0x55] = opc{"EOR", ZEPIX},
   [0x4D] = opc{"EOR", ABSOL},
   [0x5D] = opc{"EOR", ABSIX},
   [0x59] = opc{"EOR", ABSIY},
   [0x41] = opc{"EOR", INDIN},
   [0x51] = opc{"EOR", ININD},
   -- INC 
   [0xE6] = opc{"INC", ZEROP}, 
   [0xF6] = opc{"INC", ZEPIX},
   [0xEE] = opc{"INC", ABSOL},
   [0xFE] = opc{"INC", ABSIX},
   -- INX 
   [0xE8] = opc{"INX", IMPLI}, 
   -- INY 
   [0xC8] = opc{"INY", IMPLI}, 
   -- JMP 
   [0x4C] = opc{"JMP", ABSOL}, 
   [0x6C] = opc{"JMP", INDIA},
   -- JSR 
   [0x20] = opc{"JSR", ABSOL}, 
   -- LDA 
   [0xA9] = opc{"LDA", IMMED}, 
   [0xA5] = opc{"LDA", ZEROP},
   [0xB5] = opc{"LDA", ZEPIX},
   [0xAD] = opc{"LDA", ABSOL},
   [0xBD] = opc{"LDA", ABSIX},
   [0xB9] = opc{"LDA", ABSIY},
   [0xA1] = opc{"LDA", INDIN},
   [0xB1] = opc{"LDA", ININD},
   -- LDX 
   [0xA2] = opc{"LDX", IMMED}, 
   [0xA6] = opc{"LDX", ZEROP},
   [0xB6] = opc{"LDX", ZEPIY},
   [0xAE] = opc{"LDX", ABSOL},
   [0xBE] = opc{"LDX", ABSIY},
   -- LDY 
   [0xA0] = opc{"LDY", IMMED}, 
   [0xA4] = opc{"LDY", ZEROP},
   [0xB4] = opc{"LDY", ZEPIX},
   [0xAC] = opc{"LDY", ABSOL},
   [0xBC] = opc{"LDY", ABSIX},
   -- LSR 
   [0x4A] = opc{"LSR", ACCUM}, 
   [0x46] = opc{"LSR", ZEROP},
   [0x56] = opc{"LSR", ZEPIX},
   [0x4E] = opc{"LSR", ABSOL},
   [0x5E] = opc{"LSR", ABSIX},
   -- NOP 
   [0xEA] = opc{"NOP", IMPLI}, 
   -- ORA 
   [0x09] = opc{"ORA", IMMED}, 
   [0x05] = opc{"ORA", ZEROP},
   [0x15] = opc{"ORA", ZEPIX},
   [0x0D] = opc{"ORA", ABSOL},
   [0x1D] = opc{"ORA", ABSIX},
   [0x19] = opc{"ORA", ABSIY},
   [0x01] = opc{"ORA", INDIN},
   [0x11] = opc{"ORA", ININD},
   -- PHA 
   [0x48] = opc{"PHA", IMPLI}, 
   -- PHP 
   [0x08] = opc{"PHP", IMPLI}, 
   -- PLA 
   [0x68] = opc{"PLA", IMPLI}, 
   -- PLP 
   [0x28] = opc{"PLP", IMPLI}, 
   -- ROL 
   [0x2A] = opc{"ROL", ACCUM}, 
   [0x26] = opc{"ROL", ZEROP},
   [0x36] = opc{"ROL", ZEPIX},
   [0x2E] = opc{"ROL", ABSOL},
   [0x3E] = opc{"ROL", ABSIX},
   -- ROR 
   [0x6A] = opc{"ROR", ACCUM}, 
   [0x66] = opc{"ROR", ZEROP},
   [0x76] = opc{"ROR", ZEPIX},
   [0x6E] = opc{"ROR", ABSOL},
   [0x7E] = opc{"ROR", ABSIX},
   -- RTI 
   [0x40] = opc{"RTI", IMPLI}, 
   -- RTS 
   [0x60] = opc{"RTS", IMPLI}, 
   -- SBC 
   [0xE9] = opc{"SBC", IMMED}, 
   [0xE5] = opc{"SBC", ZEROP},
   [0xF5] = opc{"SBC", ZEPIX},
   [0xED] = opc{"SBC", ABSOL},
   [0xFD] = opc{"SBC", ABSIX},
   [0xF9] = opc{"SBC", ABSIY},
   [0xE1] = opc{"SBC", INDIN},
   [0xF1] = opc{"SBC", ININD},
   -- SEC 
   [0x38] = opc{"SEC", IMPLI}, 
   -- SED 
   [0xF8] = opc{"SED", IMPLI}, 
   -- SEI 
   [0x78] = opc{"SEI", IMPLI}, 
   -- STA 
   [0x85] = opc{"STA", ZEROP}, 
   [0x95] = opc{"STA", ZEPIX},
   [0x8D] = opc{"STA", ABSOL},
   [0x9D] = opc{"STA", ABSIX},
   [0x99] = opc{"STA", ABSIY},
   [0x81] = opc{"STA", INDIN},
   [0x91] = opc{"STA", ININD},
   -- STX 
   [0x86] = opc{"STX", ZEROP}, 
   [0x96] = opc{"STX", ZEPIY},
   [0x8E] = opc{"STX", ABSOL},
   -- STY 
   [0x84] = opc{"STY", ZEROP}, 
   [0x94] = opc{"STY", ZEPIX},
   [0x8C] = opc{"STY", ABSOL},
   -- TAX 
   [0xAA] = opc{"TAX", IMPLI}, 
   -- TAY 
   [0xA8] = opc{"TAY", IMPLI}, 
   -- TSX 
   [0xBA] = opc{"TSX", IMPLI}, 
   -- TXA 
   [0x8A] = opc{"TXA", IMPLI}, 
   -- TXS 
   [0x9A] = opc{"TXS", IMPLI}, 
   -- TYA 
   [0x98] = opc{"TYA", IMPLI},
}

-- Read 16-bit word.
local function r16 (buffer, pos)
   local hi = buffer[pos+1]
   local lo = buffer[pos+2]
   return bor(bit.lshift(lo, 8), hi)
end

-- Decode statement from buffer at pos. Use PC if pos is nil.
local function decode_stmt (buffer, pos)
   pc = pos or pc
   local byte = buffer[pc]
   local opcode = opcode_table[byte]
   if not opcode then
      print(("$%.4X\t.byte %.2X"):format(base + pc, byte))
      print()
      pc = pc + 1
      return pc
   end
   local name = ffi.string(opcode.name)
   if opcode.addr_mode == IMMED then
      local op1 = buffer[pc+1]
      print(("$%.4X\t%s #$%.2X"):format(base + pc, name, op1))
      pc = pc + 2
   elseif opcode.addr_mode == ABSOL then
      local word = r16(buffer, pc)
      print(("$%.4X\t%s $%.4X"):format(base + pc, name, word))
      pc = pc + 3
   elseif opcode.addr_mode == ZEROP then
      local byte = buffer[pc+1]
      print(("$%.4X\t%s $%.2X"):format(base + pc, name, byte))
      pc = pc + 2
   elseif opcode.addr_mode == IMPLI then
      print(("$%.4X\t%s"):format(base + pc, name))
      pc = pc + 1
   elseif opcode.addr_mode == INDIA then
      local word = r16(buffer, pc)
      print(("$%.4X\t%s ($%.4X)"):format(base + pc, name, word))
      pc = pc + 3
   elseif opcode.addr_mode == ABSIX then
      local word = r16(buffer, pc)
      print(("$%.4X\t%s $%.4X,X"):format(base + pc, name, word))
      pc = pc + 3
   elseif opcode.addr_mode == ABSIY then
      local word = r16(buffer, pc)
      print(("$%.4X\t%s $%.4X,Y"):format(base + pc, name, word))
      pc = pc + 3
   elseif opcode.addr_mode == ZEPIX then
      local byte = buffer[pc + 1]
      print(("$%.4X\t%s $%.2X,X"):format(base + pc, name, byte))
      pc = pc + 2
   elseif opcode.addr_mode == ZEPIY then
      local byte = buffer[pc + 1]
      print(("$%.4X\t%s $%.2X,Y"):format(base + pc, name, byte))
      pc = pc + 2
   elseif opcode.addr_mode == INDIN then
      local byte = buffer[pc+1]
      print(("$%.4X\t%s ($%.2X,X)"):format(base + pc, name, byte))
      pc = pc + 2
   elseif opcode.addr_mode == ININD then
      local byte = buffer[pc+1]
      print(("$%.4X\t%s ($%.2X),Y"):format(base + pc, name, byte))
      pc = pc + 2
   elseif opcode.addr_mode == RELAT then
      local rel_addr = buffer[pc+1]
      local abs_addr = pc + 2
      if rel_addr > 0x7F then
          abs_addr = abs_addr - (bit.band(bit.bnot(rel_addr), 0x7F) + 1)
      else
          abs_addr = abs_addr + bit.band(rel_addr, 0x7F)
      end
      print(("$%.4X\t%s $%.4X"):format(base + pc, name, base + abs_addr))
      pc = pc + 2
   elseif opcode.addr_mode == ACCUM then
      print(("$%.4X\t%s A"):format(base + pc, name))
      pc = pc + 1
   else
      error("Invalid address mode: "..opcode.addr_mode)
   end
   return pc
end

-- Get filesize.
local function filesize (fd)
   local size = fd:seek("end", 0)
   fd:seek("set", 0)
   return size
end

-- Read filename into buffer.
local function readfile (filename)
   local fd, err = io.open(filename, "rb")
   if not fd then error(err) end

   local size, step = filesize(fd), 4096
   local buffer = ffi.new("uint8_t[?]", size)
   for pos=0,size-1,step do
      local temp = fd:read(step)
      ffi.copy(buffer + pos, temp, step)
   end
   fd:close()
   return buffer, size
end

local function usage(exit_code)
   print("Usage: luajit disasm.lua <filename>")
   os.exit(exit_code)
end

-- Main program.
local function main()
   if #arg == 0 then usage(1) end
   local filename = unpack(arg)
   local buffer, size = readfile(filename)
   while pc < size do
      decode_stmt(buffer, pc)
   end
end

main()
