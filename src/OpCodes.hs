module OpCodes where

import Data.ByteString.Short (ShortByteString)

-- import Data.ByteString.Char8 as C
-- import Data.ByteString.Short

-- opcodes                         2 bytes
-- memory                       4096 bytes 4k
-- cpu registers                  16 bytes

-- from 0x000 to 0xFFF -> 4096 values - 12 bits
-- index register I                2 bytes - 16 bit
-- index register pc               2 bytes - 16 bit

-- 0x000-0x1FF - Chip 8 interpreter (contains font set in emu)  - 511      values   - 9 ? 12 bit 
-- 0x050-0x0A0 - Used for the built in 4x5 pixel font set (0-F) - 160-80   values   - 8 ? 12 bit
-- 0x200-0xFFF - Program ROM and work RAM                       - 4095-200 values   - 12 bit

-- display 64x32 booleans
-- delay timer
-- sound timer

-- stack levels 16
-- stack pointer

-- 0x0-0xf keymap 16 4bit



data OpCode a
  = OPx0NNN
  | OPx00E0
  | OPx00EE
  | OPx1NNN
  | OPx2NNN
  | OPx3XNN
  | OPx4XNN
  | OPx5XY0
  | OPx6XNN
  | OPx7XNN
  | OPx8XY0
  | OPx8XY1
  | OPx8XY2
  | OPxY3 a
  | OPx8XY4
  | OPx8XY5
  | OPxY6 a
  | OPxY7 a
  | OPxYE a
  | OPx9XY0
  | OPxANNN
  | OPxBNNN
  | OPxCXNN
  | OPxDXYN
  | OPxEX9E
  | OPxEXA1
  | OPxFX07
  | OPxFX0A
  | OPxFX15
  | OPxFX18
  | OPxFX1E
  | OPxFX29
  | OPxFX33
  | OPxFX55
  | OPxFX65

data OpCodeType
  = Call
  | Display
  | Flow
  | Cond
  | Const
  | Assig
  | BitOp
  | Math
  | MEM
  | Rand
  | KeyOp
  | Timer
  | Sound
  | BCD

type CPUReg = ShortByteString

data CPURegister
  = V0
  | V1
  | V2
  | V3
  | V4
  | V5
  | V6
  | V7
  | V8
  | V9
  | VA
  | VB
  | VC
  | VD
  | VE
  | CarryFlag
