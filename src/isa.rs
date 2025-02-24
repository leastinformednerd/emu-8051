#![allow(dead_code)]

#[derive(Debug, PartialEq, Eq)]
pub struct BitAddr {
    pub byte: u8,
    pub bit: u8,
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
        bit: u8,
        offset: i8,
    },
    JumpIfBit {
        bit: u8,
        offset: i8,
    },
    JumpIfNotBit {
        bit: u8,
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
        imm: Indirect,
    },
    MovXMemToAccIndirectR0 {
        imm: Indirect,
    },
    MovAccPcToAcc,
    MovAccDptrToAcc,

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

            _ => todo!(),
        }

        Ok(())
    }
}
