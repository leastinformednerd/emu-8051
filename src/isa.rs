#![allow(dead_code)]

#[derive(Debug, PartialEq, Eq)]
pub struct BitAddr {
    pub byte: u8,
    pub bit: u8,
}

impl BitAddr {
    pub fn as_byte(&self) -> u8 {
        if self.byte < 0x80 {
            (self.byte & 0b00011111) | ((self.bit & 0b111) << 5)
        } else {
            ((self.byte & 0b11111) << 5) | (self.bit & 0b111)
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operand {
    RegisterDirect(GeneralPurposeRegister),
    Indirect(Indirect),
    MemoryDirect(u8),
    SpecialRegister(SpecialFunctionRegister),
    ImmediateByte(u8),
    ImmediateWord(u16),
}

impl Operand {
    pub fn get_nibble(&self) -> u8 {
        match self {
            RegisterDirect(GeneralPurposeRegister::R0) => 0x8,
            RegisterDirect(GeneralPurposeRegister::R1) => 0x9,
            RegisterDirect(GeneralPurposeRegister::R2) => 0xA,
            RegisterDirect(GeneralPurposeRegister::R3) => 0xB,
            RegisterDirect(GeneralPurposeRegister::R4) => 0xC,
            RegisterDirect(GeneralPurposeRegister::R5) => 0xD,
            RegisterDirect(GeneralPurposeRegister::R6) => 0xE,
            RegisterDirect(GeneralPurposeRegister::R7) => 0xF,
            Indirect(Indirect::R0) => 0x6,
            Indirect(Indirect::R1) => 0x7,
            MemoryDirect(_) => 0x5,
            SpecialRegister(_) => 0x5,
            ImmediateByte(_) => 0x4,
            _ => panic!("Invalid operand provided, found {self:?}"),
        }
    }
}

use Operand::*;

#[derive(Debug, PartialEq, Eq)]
pub enum SpecialFunctionRegister {
    Accumulator,
    AccExtension,
    StackPointer,
    ProgramStatus,
    DPtrLow,
    DPtrHigh,
    InterruptEnable,
    InterruptPriority,
    IOPort0,
    IOPort1,
    IOPort2,
    IOPort3,
    SerialControl,
    Buffer,
    PowerControl,
    TimerControl,
    TimerMode,
    Timer0Low,
    Timer0High,
    Timer1Low,
    Timer1High,
}

use SpecialFunctionRegister::*;

impl SpecialFunctionRegister {
    pub fn addr_of(&self) -> u8 {
        match self {
            Accumulator => 0xE0,
            AccExtension => 0xF0,
            StackPointer => 0x81,
            ProgramStatus => 0xD0,
            DPtrLow => 0x82,
            DPtrHigh => 0x83,
            InterruptEnable => 0xA8,
            InterruptPriority => 0xB8,
            IOPort0 => 0x80,
            IOPort1 => 0x90,
            IOPort2 => 0xA0,
            IOPort3 => 0xB0,
            SerialControl => 0x98,
            Buffer => 0x99,
            PowerControl => 0x87,
            TimerControl => 0x88,
            TimerMode => 0x89,
            Timer0Low => 0x8A,
            Timer1Low => 0x8B,
            Timer0High => 0x8C,
            Timer1High => 0x8D,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum GeneralPurposeRegister {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Indirect {
    R0,
    R1,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    Nop,

    Increment {
        operand: Operand,
    },
    IncrementDPtr,

    Decrement {
        operand: Operand,
    },

    Add {
        src: Operand,
        dst: Operand,
        carry: bool,
    },
    Sub {
        src: Operand,
        dst: Operand,
    },

    LogicalAnd {
        src: Operand,
        dst: Operand,
    },
    LogicalOr {
        src: Operand,
        dst: Operand,
    },
    LogicalXor {
        src: Operand,
        dst: Operand,
    },

    LogicalAndCarry {
        src: BitAddr,
        complement: bool,
    },
    LogicalOrCarry {
        src: BitAddr,
        complement: bool,
    },

    Mov {
        src: Operand,
        dst: Operand,
    },

    Exchange {
        dst: Operand,
    },
    ExchangeLowNibble {
        src: Indirect,
    },

    JumpIfBitAndClear {
        bit: BitAddr,
        offset: i8,
    },
    JumpIfBit {
        bit: BitAddr,
        offset: i8,
    },
    JumpIfNotBit {
        bit: BitAddr,
        offset: i8,
    },
    JumpIfCarry {
        offset: i8,
    },
    JumpIfNotCarry {
        offset: i8,
    },
    JumpIfZero {
        offset: i8,
    },
    JumpIfNonZero {
        offset: i8,
    },
    ShortJump {
        offset: i8,
    },
    AbsoluteJump {
        addr: u16,
    },
    AbsoluteCall {
        addr: u16,
    },
    LongJump {
        addr: u16,
    },
    LongCall {
        addr: u16,
    },
    JumpAccPlusDptr,
    CompareJumpNotEqual {
        src: Operand,
        compared_to: Operand,
        offset: i8,
    },
    DecrementJumpNotZero {
        operand: Operand,
        offset: i8,
    },

    Push {
        addr: u8,
    },
    Pop {
        addr: u8,
    },

    MovImmediateDataPtr {
        value: u16,
    },
    MovAccToXMemDptr,
    MovXMemToAccDptr,
    MovAccToXMemIndirect {
        reg: Indirect,
    },
    MovXMemToAccIndirect {
        reg: Indirect,
    },
    MovCodeAccPcToAcc,
    MovCodeAccDptrToAcc,

    Return,
    ReturnFromInterrupt,

    MovBitCarry {
        bit_addr: BitAddr,
    },
    MovCarryBit {
        bit_addr: BitAddr,
    },

    ComplementBit {
        bit_addr: BitAddr,
    },
    ClearBit {
        bit_addr: BitAddr,
    },
    SetBit {
        bit_addr: BitAddr,
    },
    ComplementCarry,
    ClearCarry,
    SetCarry,

    ClearAcc,
    ComplementAcc,

    RotateAccRight,
    RotateAccRightThroughCarry,
    RotateAccLeft,
    RotateAccLeftThroughCarry,

    Mul,
    Div,

    SwapAccNibbles,

    DecimalAdjustAcc,
}

use Instruction::*;

#[derive(Debug)]
pub enum InstructionDecodeError {
    InvalidOperand(Operand),
}

macro_rules! mem_direct {
    ($buffer:expr, $op:ident) => {
        if let MemoryDirect(addr) = $op {
            $buffer.push(*addr)
        } else {
            if let SpecialRegister(reg) = $op {
                $buffer.push(reg.addr_of())
            }
        }
    };
}

impl Instruction {
    pub fn append_encoded(&self, buffer: &mut Vec<u8>) -> Result<(), InstructionDecodeError> {
        match self {
            Nop => buffer.push(0x00),
            Increment {
                operand: SpecialRegister(Accumulator),
            } => buffer.push(0x04),
            Increment {
                operand:
                    operand @ (RegisterDirect(_) | Indirect(_) | MemoryDirect(_) | SpecialRegister(_)),
            } => {
                buffer.push(0x00 & operand.get_nibble());
                mem_direct!(buffer, operand)
            }

            Decrement {
                operand: SpecialRegister(Accumulator),
            } => buffer.push(0x14),
            Decrement {
                operand:
                    operand @ (RegisterDirect(_) | Indirect(_) | MemoryDirect(_) | SpecialRegister(_)),
            } => {
                buffer.push(0x10 << 4 | operand.get_nibble());
                mem_direct!(buffer, operand)
            }

            Add {
                dst: SpecialRegister(Accumulator),
                src: src @ (RegisterDirect(_) | Indirect(_) | MemoryDirect(_) | SpecialRegister(_)),
                carry,
            } => {
                buffer.push(if *carry { 0x3 } else { 0x2 } << 4 | src.get_nibble());
                mem_direct!(buffer, src)
            }

            Add {
                dst: SpecialRegister(Accumulator),
                src: ImmediateByte(imm),
                carry,
            } => {
                buffer.push(if *carry { 0x34 } else { 0x24 });
                buffer.push(*imm);
            }

            LogicalOr {
                dst: SpecialRegister(Accumulator),
                src: src @ (RegisterDirect(_) | Indirect(_) | MemoryDirect(_) | SpecialRegister(_)),
            } => {
                buffer.push(0x4 << 4 | src.get_nibble());
                mem_direct!(buffer, src)
            }

            LogicalOr {
                dst: SpecialRegister(Accumulator),
                src: ImmediateByte(imm),
            } => {
                buffer.push(0x44);
                buffer.push(*imm)
            }

            LogicalAnd {
                dst: SpecialRegister(Accumulator),
                src: src @ (RegisterDirect(_) | Indirect(_) | MemoryDirect(_) | SpecialRegister(_)),
            } => {
                buffer.push(0x5 << 4 | src.get_nibble());
                mem_direct!(buffer, src)
            }

            LogicalAnd {
                dst: SpecialRegister(Accumulator),
                src: ImmediateByte(imm),
            } => {
                buffer.push(0x54);
                buffer.push(*imm)
            }

            LogicalXor {
                dst: SpecialRegister(Accumulator),
                src: src @ (RegisterDirect(_) | Indirect(_) | MemoryDirect(_) | SpecialRegister(_)),
            } => {
                buffer.push(0x6 << 4 | src.get_nibble());
                mem_direct!(buffer, src)
            }

            LogicalXor {
                dst: SpecialRegister(Accumulator),
                src: ImmediateByte(imm),
            } => {
                buffer.push(0x64);
                buffer.push(*imm)
            }

            Mov {
                dst: dst @ (RegisterDirect(_) | Indirect(_)),
                src: ImmediateByte(imm),
            } => {
                buffer.push(0x7 << 4 | dst.get_nibble());
                buffer.push(*imm);
            }

            Mov {
                dst: SpecialRegister(Accumulator),
                src: src @ (RegisterDirect(_) | SpecialRegister(_)),
            } => {
                buffer.push(0xE << 4 | src.get_nibble());
                mem_direct!(buffer, src)
            }

            Mov {
                src: SpecialRegister(Accumulator),
                dst: dst @ (RegisterDirect(_) | Indirect(_) | MemoryDirect(_) | SpecialRegister(_)),
            } => {
                buffer.push(0xF << 4 | dst.get_nibble());
                mem_direct!(buffer, dst)
            }

            Mov {
                dst: SpecialRegister(Accumulator),
                src: ImmediateByte(imm),
            } => {
                buffer.push(0x74);
                buffer.push(*imm)
            }

            Mov {
                dst: dst @ (MemoryDirect(_) | SpecialRegister(_)),
                src: src @ (RegisterDirect(_) | Indirect(_) | MemoryDirect(_) | SpecialRegister(_)),
            } => {
                buffer.push(0x8 << 4 | src.get_nibble());
                mem_direct!(buffer, dst);
                mem_direct!(buffer, src)
            }

            Sub {
                dst: SpecialRegister(Accumulator),
                src: src @ (RegisterDirect(_) | Indirect(_) | MemoryDirect(_) | SpecialRegister(_)),
            } => {
                buffer.push(0x9 << 4 | src.get_nibble());
                mem_direct!(buffer, src)
            }

            Mov {
                dst: dst @ (RegisterDirect(_) | Indirect(_)),
                src: src @ (MemoryDirect(_) | SpecialRegister(_)),
            } => {
                buffer.push(0xA << 4 | dst.get_nibble());
                mem_direct!(buffer, dst);
                mem_direct!(buffer, src)
            }

            CompareJumpNotEqual {
                src: src @ (RegisterDirect(_) | Indirect(_)),
                compared_to: ImmediateByte(imm),
                offset,
            } => {
                buffer.push(0xB << 4 | src.get_nibble());
                buffer.push(*imm);
                buffer.push(*offset as u8)
            }

            CompareJumpNotEqual {
                src: SpecialRegister(Accumulator),
                compared_to: ImmediateByte(imm),
                offset,
            } => {
                buffer.push(0xB4);
                buffer.push(*imm);
                buffer.push(*offset as u8)
            }

            Exchange {
                dst: dst @ (RegisterDirect(_) | Indirect(_) | MemoryDirect(_) | SpecialRegister(_)),
            } => {
                buffer.push(0xC << 4 | dst.get_nibble());
                mem_direct!(buffer, dst)
            }

            DecrementJumpNotZero {
                operand: operand @ (RegisterDirect(_) | MemoryDirect(_) | SpecialRegister(_)),
                offset,
            } => {
                buffer.push(0xD << 4 | operand.get_nibble());
                mem_direct!(buffer, operand);
                buffer.push(*offset as u8);
            }

            JumpIfBitAndClear { bit, offset } => {
                buffer.push(0x10);
                buffer.push(bit.as_byte());
                buffer.push(*offset as u8)
            }

            JumpIfBit { bit, offset } => {
                buffer.push(0x20);
                buffer.push(bit.as_byte());
                buffer.push(*offset as u8)
            }

            JumpIfNotBit { bit, offset } => {
                buffer.push(0x30);
                buffer.push(bit.as_byte());
                buffer.push(*offset as u8)
            }

            JumpIfCarry { offset } => {
                buffer.push(0x40);
                buffer.push(*offset as u8)
            }

            JumpIfNotCarry { offset } => {
                buffer.push(0x50);
                buffer.push(*offset as u8)
            }

            JumpIfZero { offset } => {
                buffer.push(0x60);
                buffer.push(*offset as u8);
            }

            JumpIfNonZero { offset } => {
                buffer.push(0x70);
                buffer.push(*offset as u8)
            }

            ShortJump { offset } => {
                buffer.push(0x80);
                buffer.push(*offset as u8)
            }

            MovImmediateDataPtr { value } => {
                buffer.push(0x90);
                buffer.extend_from_slice(&value.to_le_bytes())
            }

            LogicalOrCarry { src, complement } => {
                buffer.push(if *complement { 0xA0 } else { 0x72 });
                buffer.push(src.as_byte());
            }

            LogicalAndCarry { src, complement } => {
                buffer.push(if *complement { 0xB0 } else { 0x82 });
                buffer.push(src.as_byte());
            }

            Push { addr } => {
                buffer.push(0xC0);
                buffer.push(*addr)
            }

            Pop { addr } => {
                buffer.push(0xD0);
                buffer.push(*addr)
            }

            MovXMemToAccDptr => {
                buffer.push(0xE0);
            }

            MovAccToXMemDptr => {
                buffer.push(0xF0);
            }

            AbsoluteJump { addr } => {
                assert!(
                    *addr < (1 << 12),
                    "Addr must be less than 2**12 found {addr:x}"
                );
                buffer.push(0x1 | ((addr & (0b111 << 8) >> 4) as u8));
                buffer.push((addr & 0xff) as u8);
            }

            AbsoluteCall { addr } => {
                assert!(
                    *addr < (1 << 12),
                    "Addr must be less than 2**12 found {addr:x}"
                );
                buffer.push(0x11 | ((addr & (0b111 << 8) >> 4) as u8));
                buffer.push((addr & 0xff) as u8);
            }

            LongJump { addr } => {
                buffer.push(0x02);
                buffer.extend_from_slice(&addr.to_le_bytes())
            }

            LongCall { addr } => {
                buffer.push(0x12);
                buffer.extend_from_slice(&addr.to_le_bytes())
            }

            Return => buffer.push(0x22),

            ReturnFromInterrupt => buffer.push(0x32),

            MovCarryBit { bit_addr } => {
                buffer.push(0x92);
                buffer.push(bit_addr.as_byte())
            }

            MovBitCarry { bit_addr } => {
                buffer.push(0xA2);
                buffer.push(bit_addr.as_byte())
            }

            ComplementBit { bit_addr } => {
                buffer.push(0xB2);
                buffer.push(bit_addr.as_byte())
            }

            ClearBit { bit_addr } => {
                buffer.push(0xC2);
                buffer.push(bit_addr.as_byte())
            }

            SetBit { bit_addr } => {
                buffer.push(0xD2);
                buffer.push(bit_addr.as_byte())
            }

            MovXMemToAccIndirect { reg: Indirect::R0 } => {
                buffer.push(0xE2);
            }

            MovAccToXMemIndirect { reg: Indirect::R0 } => {
                buffer.push(0xF2);
            }

            MovXMemToAccIndirect { reg: Indirect::R1 } => {
                buffer.push(0xE3);
            }

            MovAccToXMemIndirect { reg: Indirect::R1 } => {
                buffer.push(0xF3);
            }

            RotateAccRight => buffer.push(0x03),
            RotateAccRightThroughCarry => buffer.push(0x13),
            RotateAccLeft => buffer.push(0x23),
            RotateAccLeftThroughCarry => buffer.push(0x33),

            JumpAccPlusDptr => buffer.push(0x73),

            MovCodeAccPcToAcc => buffer.push(0x83),
            MovCodeAccDptrToAcc => buffer.push(0x93),

            IncrementDPtr => buffer.push(0xA3),

            ComplementCarry => buffer.push(0xB3),
            ClearCarry => buffer.push(0xC3),
            SetCarry => buffer.push(0xD3),

            Div => buffer.push(0x84),
            Mul => buffer.push(0xA4),

            SwapAccNibbles => buffer.push(0xC4),

            DecimalAdjustAcc => buffer.push(0xD4),

            ClearAcc => buffer.push(0xE4),
            ComplementAcc => buffer.push(0xF4),

            ExchangeLowNibble { src: Indirect::R0 } => buffer.push(0xD6),
            ExchangeLowNibble { src: Indirect::R1 } => buffer.push(0xD7),

            invalid => panic!("Found an invalid operation {:?}", invalid),
        }

        Ok(())
    }
}
