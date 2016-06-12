#!/usr/bin/env luajit

local ffi = require("ffi")
local bit = require("bit")

local bor = bit.bor

local longopts = {
   help=0,
   base=1,
   num_bytes=1,
   dump=0,
   cycle_count=0,
}

local shortopts = {
   h=0,
   b=1,
   n=1,
   d=0,
   c=0,
}

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

local CYCLES_CROSS_PAGE_ADDS_ONE = 1
local CYCLES_BRANCH_TAKEN_ADDS_ONE = 2

local pc = 0
local base = 0x8000

local opcode_t = ffi.typeof[[
   struct {
      const char *name;            // Opcode name.
      uint8_t addr_mode;           // Addressing mode.
      uint8_t cycles;              // Cycles per opcode.
      uint8_t cycles_exceptions;   // Mask of cycle-counting exceptions.
   } 
]]
                
local function opc (t)
   assert(type(t) == "table")
   return ffi.new(opcode_t, t)
end             

-- List of opcodes.
local opcode_table = {
   -- ADC.
   [0x69] = opc{"ADC", IMMED, 2, 0},
   [0x65] = opc{"ADC", ZEROP, 3, 0},
   [0x75] = opc{"ADC", ZEPIX, 4, 0},
   [0x6D] = opc{"ADC", ABSOL, 4, 0},
   [0x7D] = opc{"ADC", ABSIX, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0x79] = opc{"ADC", ABSIY, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0x61] = opc{"ADC", INDIN, 6, 0},
   [0x71] = opc{"ADC", ININD, 5, CYCLES_CROSS_PAGE_ADDS_ONE},
   -- AND.
   [0x29] = opc{"AND", IMMED, 2, 0},
   [0x25] = opc{"AND", ZEROP, 3, 0},
   [0x35] = opc{"AND", ZEPIX, 4, 0},
   [0x2D] = opc{"AND", ABSOL, 4, 0},
   [0x3D] = opc{"AND", ABSIX, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0x39] = opc{"AND", ABSIY, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0x21] = opc{"AND", INDIN, 6, 0},
   [0x31] = opc{"AND", ININD, 5, CYCLES_CROSS_PAGE_ADDS_ONE},
   -- ASL.
   [0x0A] = opc{"ASL", ACCUM, 2, 0},
   [0x06] = opc{"ASL", ZEROP, 5, 0},
   [0x16] = opc{"ASL", ZEPIX, 6, 0},
   [0x0E] = opc{"ASL", ABSOL, 6, 0},
   [0x1E] = opc{"ASL", ABSIX, 7, 0},
   -- BCC 
   [0x90] = opc{"BCC", RELAT, 2, bor(CYCLES_CROSS_PAGE_ADDS_ONE,  CYCLES_BRANCH_TAKEN_ADDS_ONE)},
   -- BCS 
   [0xB0] = opc{"BCS", RELAT, 2, bor(CYCLES_CROSS_PAGE_ADDS_ONE, CYCLES_BRANCH_TAKEN_ADDS_ONE)},
   -- BEQ 
   [0xF0] = opc{"BEQ", RELAT, 2, bor(CYCLES_CROSS_PAGE_ADDS_ONE, CYCLES_BRANCH_TAKEN_ADDS_ONE)},
    -- BIT 
   [0x24] = opc{"BIT", ZEROP, 3, 0},
   [0x2C] = opc{"BIT", ABSOL, 4, 0},
   -- BMI 
   [0x30] = opc{"BMI", RELAT, 2, bor(CYCLES_CROSS_PAGE_ADDS_ONE, CYCLES_BRANCH_TAKEN_ADDS_ONE)},
   -- BNE 
   [0xD0] = opc{"BNE", RELAT, 2, bor(CYCLES_CROSS_PAGE_ADDS_ONE, CYCLES_BRANCH_TAKEN_ADDS_ONE)},
   -- BPL 
   [0x10] = opc{"BPL", RELAT, 2, bor(CYCLES_CROSS_PAGE_ADDS_ONE, CYCLES_BRANCH_TAKEN_ADDS_ONE)},
   -- BRK 
   [0x00] = opc{"BRK", IMPLI, 7, 0},
   -- BVC 
   [0x50] = opc{"BVC", RELAT, 2, bor(CYCLES_CROSS_PAGE_ADDS_ONE, CYCLES_BRANCH_TAKEN_ADDS_ONE)},
   -- BVS 
   [0x70] = opc{"BVS", RELAT, 2, bor(CYCLES_CROSS_PAGE_ADDS_ONE, CYCLES_BRANCH_TAKEN_ADDS_ONE)},
   -- CLC 
   [0x18] = opc{"CLC", IMPLI, 2, 0}, 
   -- CLD 
   [0xD8] = opc{"CLD", IMPLI, 2, 0}, 
   -- CLI 
   [0x58] = opc{"CLI", IMPLI, 2, 0}, 
   -- CLV 
   [0xB8] = opc{"CLV", IMPLI, 2, 0}, 
   -- CMP 
   [0xC9] = opc{"CMP", IMMED, 2, 0}, 
   [0xC5] = opc{"CMP", ZEROP, 3, 0},
   [0xD5] = opc{"CMP", ZEPIX, 4, 0},
   [0xCD] = opc{"CMP", ABSOL, 4, 0},
   [0xDD] = opc{"CMP", ABSIX, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0xD9] = opc{"CMP", ABSIY, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0xC1] = opc{"CMP", INDIN, 6, 0},
   [0xD1] = opc{"CMP", ININD, 5, CYCLES_CROSS_PAGE_ADDS_ONE},
   -- CPX 
   [0xE0] = opc{"CPX", IMMED, 2, 0}, 
   [0xE4] = opc{"CPX", ZEROP, 3, 0},
   [0xEC] = opc{"CPX", ABSOL, 4, 0},
   -- CPY 
   [0xC0] = opc{"CPY", IMMED, 2, 0}, 
   [0xC4] = opc{"CPY", ZEROP, 3, 0},
   [0xCC] = opc{"CPY", ABSOL, 4, 0},
   -- DEC 
   [0xC6] = opc{"DEC", ZEROP, 5, 0}, 
   [0xD6] = opc{"DEC", ZEPIX, 6, 0},
   [0xCE] = opc{"DEC", ABSOL, 6, 0},
   [0xDE] = opc{"DEC", ABSIX, 7, 0},
   -- DEX 
   [0xCA] = opc{"DEX", IMPLI, 2, 0}, 
   -- DEY 
   [0x88] = opc{"DEY", IMPLI, 2, 0}, 
   -- EOR 
   [0x49] = opc{"EOR", IMMED, 2, 0}, 
   [0x45] = opc{"EOR", ZEROP, 3, 0},
   [0x55] = opc{"EOR", ZEPIX, 4, 0},
   [0x4D] = opc{"EOR", ABSOL, 4, 0},
   [0x5D] = opc{"EOR", ABSIX, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0x59] = opc{"EOR", ABSIY, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0x41] = opc{"EOR", INDIN, 6, 1},
   [0x51] = opc{"EOR", ININD, 5, CYCLES_CROSS_PAGE_ADDS_ONE},
   -- INC 
   [0xE6] = opc{"INC", ZEROP, 5, 0}, 
   [0xF6] = opc{"INC", ZEPIX, 6, 0},
   [0xEE] = opc{"INC", ABSOL, 6, 0},
   [0xFE] = opc{"INC", ABSIX, 7, 0},
   -- INX 
   [0xE8] = opc{"INX", IMPLI, 2, 0}, 
   -- INY 
   [0xC8] = opc{"INY", IMPLI, 2, 0}, 
   -- JMP 
   [0x4C] = opc{"JMP", ABSOL, 3, 0}, 
   [0x6C] = opc{"JMP", INDIA, 5, 0},
   -- JSR 
   [0x20] = opc{"JSR", ABSOL, 6, 0}, 
   -- LDA 
   [0xA9] = opc{"LDA", IMMED, 2, 0}, 
   [0xA5] = opc{"LDA", ZEROP, 3, 0},
   [0xB5] = opc{"LDA", ZEPIX, 4, 0},
   [0xAD] = opc{"LDA", ABSOL, 4, 0},
   [0xBD] = opc{"LDA", ABSIX, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0xB9] = opc{"LDA", ABSIY, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0xA1] = opc{"LDA", INDIN, 6, 0},
   [0xB1] = opc{"LDA", ININD, 5, CYCLES_CROSS_PAGE_ADDS_ONE},
   -- LDX 
   [0xA2] = opc{"LDX", IMMED, 2, 0}, 
   [0xA6] = opc{"LDX", ZEROP, 3, 0},
   [0xB6] = opc{"LDX", ZEPIY, 4, 0},
   [0xAE] = opc{"LDX", ABSOL, 4, 0},
   [0xBE] = opc{"LDX", ABSIY, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   -- LDY 
   [0xA0] = opc{"LDY", IMMED, 2, 0}, 
   [0xA4] = opc{"LDY", ZEROP, 3, 0},
   [0xB4] = opc{"LDY", ZEPIX, 4, 0},
   [0xAC] = opc{"LDY", ABSOL, 4, 0},
   [0xBC] = opc{"LDY", ABSIX, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   -- LSR 
   [0x4A] = opc{"LSR", ACCUM, 2, 0}, 
   [0x46] = opc{"LSR", ZEROP, 5, 0},
   [0x56] = opc{"LSR", ZEPIX, 6, 0},
   [0x4E] = opc{"LSR", ABSOL, 6, 0},
   [0x5E] = opc{"LSR", ABSIX, 7, 0},
   -- NOP 
   [0xEA] = opc{"NOP", IMPLI, 2, 0}, 
   -- ORA 
   [0x09] = opc{"ORA", IMMED, 2, 0}, 
   [0x05] = opc{"ORA", ZEROP, 3, 0},
   [0x15] = opc{"ORA", ZEPIX, 4, 0},
   [0x0D] = opc{"ORA", ABSOL, 4, 0},
   [0x1D] = opc{"ORA", ABSIX, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0x19] = opc{"ORA", ABSIY, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0x01] = opc{"ORA", INDIN, 6, 0},
   [0x11] = opc{"ORA", ININD, 5, CYCLES_CROSS_PAGE_ADDS_ONE},
   -- PHA 
   [0x48] = opc{"PHA", IMPLI, 3, 0}, 
   -- PHP 
   [0x08] = opc{"PHP", IMPLI, 3, 0}, 
   -- PLA 
   [0x68] = opc{"PLA", IMPLI, 4, 0}, 
   -- PLP 
   [0x28] = opc{"PLP", IMPLI, 4, 0}, 
   -- ROL 
   [0x2A] = opc{"ROL", ACCUM, 2, 0}, 
   [0x26] = opc{"ROL", ZEROP, 5, 0},
   [0x36] = opc{"ROL", ZEPIX, 6, 0},
   [0x2E] = opc{"ROL", ABSOL, 6, 0},
   [0x3E] = opc{"ROL", ABSIX, 7, 0},
   -- ROR 
   [0x6A] = opc{"ROR", ACCUM, 2, 0}, 
   [0x66] = opc{"ROR", ZEROP, 5, 0},
   [0x76] = opc{"ROR", ZEPIX, 6, 0},
   [0x6E] = opc{"ROR", ABSOL, 6, 0},
   [0x7E] = opc{"ROR", ABSIX, 7, 0},
   -- RTI 
   [0x40] = opc{"RTI", IMPLI, 6, 0}, 
   -- RTS 
   [0x60] = opc{"RTS", IMPLI, 6, 0}, 
   -- SBC 
   [0xE9] = opc{"SBC", IMMED, 2, 0}, 
   [0xE5] = opc{"SBC", ZEROP, 3, 0},
   [0xF5] = opc{"SBC", ZEPIX, 4, 0},
   [0xED] = opc{"SBC", ABSOL, 4, 0},
   [0xFD] = opc{"SBC", ABSIX, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0xF9] = opc{"SBC", ABSIY, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0xE1] = opc{"SBC", INDIN, 6, 0},
   [0xF1] = opc{"SBC", ININD, 5, CYCLES_CROSS_PAGE_ADDS_ONE},
   -- SEC 
   [0x38] = opc{"SEC", IMPLI, 2, 0}, 
   -- SED 
   [0xF8] = opc{"SED", IMPLI, 2, 0}, 
   -- SEI 
   [0x78] = opc{"SEI", IMPLI, 2, 0}, 
   -- STA 
   [0x85] = opc{"STA", ZEROP, 3, 0}, 
   [0x95] = opc{"STA", ZEPIX, 4, 0},
   [0x8D] = opc{"STA", ABSOL, 4, 0},
   [0x9D] = opc{"STA", ABSIX, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0x99] = opc{"STA", ABSIY, 4, CYCLES_CROSS_PAGE_ADDS_ONE},
   [0x81] = opc{"STA", INDIN, 6, 0},
   [0x91] = opc{"STA", ININD, 5, CYCLES_CROSS_PAGE_ADDS_ONE},
   -- STX 
   [0x86] = opc{"STX", ZEROP, 3, 0}, 
   [0x96] = opc{"STX", ZEPIY, 4, 0},
   [0x8E] = opc{"STX", ABSOL, 4, 0},
   -- STY 
   [0x84] = opc{"STY", ZEROP, 3, 0}, 
   [0x94] = opc{"STY", ZEPIX, 4, 0},
   [0x8C] = opc{"STY", ABSOL, 4, 0},
   -- TAX 
   [0xAA] = opc{"TAX", IMPLI, 2, 0}, 
   -- TAY 
   [0xA8] = opc{"TAY", IMPLI, 2, 0}, 
   -- TSX 
   [0xBA] = opc{"TSX", IMPLI, 2, 0}, 
   -- TXA 
   [0x8A] = opc{"TXA", IMPLI, 2, 0}, 
   -- TXS 
   [0x9A] = opc{"TXS", IMPLI, 2, 0}, 
   -- TYA 
   [0x98] = opc{"TYA", IMPLI, 2, 0},
}

-- Read 16-bit word.
local function r16 (buffer, pos)
   local hi = buffer[pos+1]
   local lo = buffer[pos+2]
   return bor(bit.lshift(lo, 8), hi)
end

Line = {}

function Line.new (opts)
   local o = {
      _dump = opts.dump,
      _cycle_count = opts.cycle_count,
      output = {},
   }
   return setmetatable(o, { __index = Line })
end

function Line:push (text)
   table.insert(self.output, text)
end

function Line:address (addr)
   self:push(("$%.4X"):format(addr))
end

function Line:instr (text)
   self:push(text)
   if #text < 8 then self:push("   ") end
end

function Line:dump (byte, word)
   if not self._dump then return end
   local text = ("%.2X"):format(byte)
   if word then
      if word < 256 then
         text = text..(" %.2X"):format(word)
      else
         text = text..(" %.4X"):format(word)
      end
   else
      text = text.."   "
   end
   text = text.."\t"
   self:push(text)
end

function Line:cycle_count (opcode)
   if not self._cycle_count then return end
   self:push(("Cycle count: %d"):format(opcode.cycles))
end

function Line:flush ()
   print(table.concat(self.output, "\t"))
end

-- Decode statement from buffer at pos. Use PC if pos is nil.
local function decode_stmt (buffer, pos, opts)
   pc = pos or pc
   local line = Line.new(opts)
   local byte = buffer[pc]
   local opcode = opcode_table[byte]
   if not opcode then
      line:address(base + pc)
      line:dump(buffer[pc])
      line:instr((".byte %.2X\n"):format(byte))
      pc = pc + 1
      return pc
   end
   local name = ffi.string(opcode.name)
   if opcode.addr_mode == IMMED then
      local op1 = buffer[pc+1]
      line:address(base + pc)
      line:dump(buffer[pc], op1)
      line:instr(("%s #$%.2X"):format(name, op1))
      pc = pc + 2
   elseif opcode.addr_mode == ABSOL then
      local word = r16(buffer, pc)
      line:address(base + pc)
      line:dump(buffer[pc], word)
      line:instr(("%s $%.4X"):format(name, word))
      pc = pc + 3
   elseif opcode.addr_mode == ZEROP then
      local byte = buffer[pc+1]
      line:address(base + pc)
      line:dump(buffer[pc], byte)
      line:instr(("%s $%.2X"):format(name, byte))
      pc = pc + 2
   elseif opcode.addr_mode == IMPLI then
      line:address(base + pc)
      line:dump(buffer[pc])
      line:instr(("%s"):format(name))
      pc = pc + 1
   elseif opcode.addr_mode == INDIA then
      local word = r16(buffer, pc)
      line:address(base + pc)
      line:dump(buffer[pc], word)
      line:instr(("%s ($%.4X)"):format(name, word))
      pc = pc + 3
   elseif opcode.addr_mode == ABSIX then
      local word = r16(buffer, pc)
      line:address(base + pc)
      line:dump(buffer[pc], word)
      line:instr(("%s $%.4X,X"):format(name, word))
      pc = pc + 3
   elseif opcode.addr_mode == ABSIY then
      local word = r16(buffer, pc)
      line:address(base + pc)
      line:dump(buffer[pc], word)
      line:instr(("%s $%.4X,Y"):format(name, word))
      pc = pc + 3
   elseif opcode.addr_mode == ZEPIX then
      local byte = buffer[pc + 1]
      line:address(base + pc)
      line:dump(buffer[pc], byte)
      line:instr(("%s $%.2X,X"):format(name, byte))
      pc = pc + 2
   elseif opcode.addr_mode == ZEPIY then
      local byte = buffer[pc + 1]
      line:address(base + pc)
      line:dump(buffer[pc], byte)
      line:instr(("%s $%.2X,Y"):format(name, byte))
      pc = pc + 2
   elseif opcode.addr_mode == INDIN then
      local byte = buffer[pc+1]
      line:address(base + pc)
      line:dump(buffer[pc], byte)
      line:instr(("%s ($%.2X,X)"):format(name, byte))
      pc = pc + 2
   elseif opcode.addr_mode == ININD then
      local byte = buffer[pc+1]
      line:address(base + pc)
      line:dump(buffer[pc], byte)
      line:instr(("%s ($%.2X),Y"):format(name, byte))
      pc = pc + 2
   elseif opcode.addr_mode == RELAT then
      local rel_addr = buffer[pc+1]
      local abs_addr = pc + 2
      if rel_addr > 0x7F then
          abs_addr = abs_addr - (bit.band(bit.bnot(rel_addr), 0x7F) + 1)
      else
          abs_addr = abs_addr + bit.band(rel_addr, 0x7F)
      end
      line:address(base + pc)
      line:dump(buffer[pc], base + abs_addr)
      line:instr(("%s $%.4X"):format(name, base + abs_addr))
      pc = pc + 2
   elseif opcode.addr_mode == ACCUM then
      line:address(base + pc)
      line:dump(buffer[pc])
      line:instr(("%s A"):format(name))
      pc = pc + 1
   else
      error("Invalid address mode: "..opcode.addr_mode)
   end

   -- Add info about cycle counting if needed.
   line:cycle_count(opcode)

   line:flush()

   return pc
end

-- Get filesize.
local function filesize (fd)
   local size = fd:seek("end", 0)
   fd:seek("set", 0)
   return size
end

-- Read filename into buffer.
local function readfile (filename, size)
   local fd, err = io.open(filename, "rb")
   if not fd then error(err) end

   local fsize, step = filesize(fd), 4096
   size = math.min(fsize, size or fsize)
   local buffer = ffi.new("uint8_t[?]", size)

   for pos=0,size,step do
      if (size - pos) < step then step = size - pos end
      local temp = fd:read(step)
      ffi.copy(buffer + pos, temp, step)
   end
   fd:close()
   return buffer, size
end

local function include (filename)
   local fd, errno = io.open(filename)
   if not fd then error(errno) end
   local content = fd:read("*all")
   fd:close()
   return content
end

local function usage(exit_code)
   print(include("USAGE"))
   os.exit(exit_code)
end

local function filter_options (args)
   local sopts, lopts, ret = {}, {}, {}
   local i = 1
   while i <= #args do
      local arg = args[i]
      -- Is long option?
      if arg:sub(1,2) == "--" then
         local opt = arg:sub(3)
         local nargs = assert(longopts[opt], ("Unknown option: --%s"):format(opt))
         if nargs == 0 then
            lopts[opt] = true
         else
            lopts[opt] = args[i+1]
            i = i + 1
         end
      -- Is s option?
      elseif arg:sub(1, 1) == "-" then
         local opt = arg:sub(2)
         local nargs = assert(shortopts[opt], ("Unknown option: -%s"):format(opt))
         if nargs == 0 then
            sopts[opt] = true
         else
            sopts[opt] = args[i+1]
            i = i + 1
         end
      else
      -- Is argument.
         table.insert(ret, arg)
      end
      i = i + 1
   end
   return sopts, lopts, ret
end

local function fatal (...)
   print(...)
   usage(1)
end

local function process_option_arguments (args, handlers)
   local shortopts, longopts, args = filter_options(args)
   for opt, val in pairs(shortopts) do
      if handlers[opt] then
         local fn = handlers[opt]
         fn(val)
      else
         fatal(("Unknown option: %s"):format("-"..opt))
      end
   end
   for opt, val in pairs(longopts) do
      if handlers[opt] then
         local fn = handlers[opt]
         fn(val)
      else
         fatal(("Unknown option: %s"):format("--"..opt))
      end
   end
   return args
end

local function parse_args(args)
   local handlers = {}
   local opts = {}
   handlers.help = function()
      usage(0)
   end
   handlers.base = function (arg)
      base = assert(tonumber(arg), "base must be a number")
   end
   handlers.num_bytes = function (arg)
      opts.num_bytes = assert(tonumber(arg), "num_bytes must be a number")
   end
   handlers.dump = function ()
      opts.dump = true
   end
   handlers.cycle_count = function ()
      opts.cycle_count = true
   end
   handlers.h = handlers.help
   handlers.b = handlers.base
   handlers.n = handlers.num_bytes
   handlers.d = handlers.dump
   handlers.c = handlers.cycle_count
   local args = process_option_arguments(args, handlers)
   if #args == 0 then usage(1) end
   return opts, args
end

-- Main program.
local function main(args)
   local opts, args = parse_args(args)
   local filename = unpack(args)
   local buffer, size = readfile(filename, opts.num_bytes)
   while pc < size do
      decode_stmt(buffer, pc, opts)
   end
end

main(arg)
