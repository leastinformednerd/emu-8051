#![allow(dead_code)]

#[derive(Debug, PartialEq, Eq)]
pub struct BitAddr {
    pub byte: u8,
    pub bit: u8,
}

impl BitAddr {
    pub fn as_byte(&self) -> u8 {
        assert!(
            self.bit < 8,
            "Expected self.bit to be less than 8, found {}",
            self.bit
        );
        match self.byte {
            0x20..0x30 => self.byte | (self.bit << 5),
            0x80 | 0x88 | 0x90 | 0x98 | 0xA0 | 0xA8 | 0xB0 | 0xB8 | 0xD0 | 0xE0 | 0xF0 => {
                self.byte | self.bit
            }
            _ => panic!("Tried to create a bit address to a non-bit adressable region"),
        }
    }

    pub fn from_byte(byte: u8) -> Self {
        match byte {
            0x00..=0x7F => BitAddr {
                byte: byte / 16,
                bit: byte % 16,
            },
            0x80..0xC0 | 0xD0..0xD8 | 0xE0..0xE8 | 0xF0..0xF8 => BitAddr {
                byte: byte & 0b11111000,
                bit: byte & 0b111,
            },
            _ => panic!("Tried to create a bit address from an invalid byte, {byte}"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

    pub fn from_byte<const ACCEPTS_IMMEDIATE: bool, ByteStream: Iterator<Item = u8>>(
        byte: u8,
        bytes: &mut ByteStream,
    ) -> Result<Self, InstructionDecodeError> {
        Self::from_nibble::<ACCEPTS_IMMEDIATE, ByteStream>(byte & 0xF, bytes)
    }

    pub fn from_nibble<const ACCEPTS_IMMEDIATE: bool, ByteStream: Iterator<Item = u8>>(
        nibble: u8,
        bytes: &mut ByteStream,
    ) -> Result<Self, InstructionDecodeError> {
        assert!(nibble < 0x10, "Expected a low nibble found {nibble:x}");
        Ok(match nibble {
            0x4 => {
                if ACCEPTS_IMMEDIATE {
                    Operand::ImmediateByte(
                        bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?,
                    )
                } else {
                    Operand::SpecialRegister(SpecialFunctionRegister::Accumulator)
                }
            }
            0x5 => {
                Operand::MemoryDirect(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?)
            }
            0x6 => Operand::Indirect(Indirect::R0),
            0x7 => Operand::Indirect(Indirect::R1),
            0x8 => Operand::RegisterDirect(GeneralPurposeRegister::R0),
            0x9 => Operand::RegisterDirect(GeneralPurposeRegister::R1),
            0xA => Operand::RegisterDirect(GeneralPurposeRegister::R2),
            0xB => Operand::RegisterDirect(GeneralPurposeRegister::R3),
            0xC => Operand::RegisterDirect(GeneralPurposeRegister::R4),
            0xD => Operand::RegisterDirect(GeneralPurposeRegister::R5),
            0xE => Operand::RegisterDirect(GeneralPurposeRegister::R6),
            0xF => Operand::RegisterDirect(GeneralPurposeRegister::R7),
            _ => panic!("Expected regular low nibble, found {nibble:x}"),
        })
    }

    pub fn normalised_register(&self) -> Self {
        SpecialRegister(match self {
            MemoryDirect(0x80) => IOPort0,
            MemoryDirect(0x81) => StackPointer,
            MemoryDirect(0x82) => DPtrLow,
            MemoryDirect(0x83) => DPtrHigh,
            MemoryDirect(0x87) => PowerControl,
            MemoryDirect(0x88) => TimerControl,
            MemoryDirect(0x89) => TimerMode,
            MemoryDirect(0x8A) => Timer0Low,
            MemoryDirect(0x8B) => Timer1Low,
            MemoryDirect(0x8C) => Timer0High,
            MemoryDirect(0x8D) => Timer1High,
            MemoryDirect(0x90) => IOPort1,
            MemoryDirect(0x98) => SerialControl,
            MemoryDirect(0x99) => Buffer,
            MemoryDirect(0xA0) => IOPort2,
            MemoryDirect(0xA8) => InterruptEnable,
            MemoryDirect(0xB0) => IOPort3,
            MemoryDirect(0xB8) => InterruptPriority,
            MemoryDirect(0xD0) => ProgramStatus,
            MemoryDirect(0xE0) => Accumulator,
            MemoryDirect(0xF0) => AccExtension,
            _ => return self.clone(),
        })
    }
}

use Operand::*;

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

impl GeneralPurposeRegister {
    pub fn num(&self) -> u8 {
        use GeneralPurposeRegister::*;
        match self {
            R0 => 0,
            R1 => 1,
            R2 => 2,
            R3 => 3,
            R4 => 4,
            R5 => 5,
            R6 => 6,
            R7 => 7,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
        dst: Indirect,
    },
    MovXMemToAccIndirect {
        src: Indirect,
    },
    MovCodeAccPcToAcc,
    MovCodeAccDptrToAcc,

    Return,
    ReturnFromInterrupt,

    MovBitCarry {
        src: BitAddr,
    },
    MovCarryBit {
        dst: BitAddr,
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
pub enum InstructionEncodeError {
    InvalidInstruction(String),
}

#[derive(Debug)]
pub enum InstructionDecodeError {
    CompletelyExhausted,
    NotEnoughBytes,
    InvalidByte,
}

/// Why did I make this a macro instead of a function?
/// If it gets inlined it's literally identical :(
/// I'm too lazy to change it now...
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
    pub fn append_encoded(&self, buffer: &mut Vec<u8>) -> Result<(), InstructionEncodeError> {
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

            MovCarryBit { dst: bit_addr } => {
                buffer.push(0x92);
                buffer.push(bit_addr.as_byte())
            }

            MovBitCarry { src: bit_addr } => {
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

            MovXMemToAccIndirect { src: Indirect::R0 } => {
                buffer.push(0xE2);
            }

            MovAccToXMemIndirect { dst: Indirect::R0 } => {
                buffer.push(0xF2);
            }

            MovXMemToAccIndirect { src: Indirect::R1 } => {
                buffer.push(0xE3);
            }

            MovAccToXMemIndirect { dst: Indirect::R1 } => {
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

            invalid => {
                return Err(InstructionEncodeError::InvalidInstruction(format!(
                    "{invalid:?}"
                )))
            }
        }

        Ok(())
    }

    fn decode_one<ByteSteam: Iterator<Item = u8>>(
        bytes: &mut ByteSteam,
    ) -> Result<Instruction, InstructionDecodeError> {
        Ok({
            let byte = bytes
                .next()
                .ok_or(InstructionDecodeError::CompletelyExhausted)?;

            match byte {
                0x00 => Nop,
                0x10 => JumpIfBitAndClear { bit: BitAddr::from_byte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?), offset: bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as i8},
                0x20 => JumpIfBit { bit: BitAddr::from_byte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?), offset: bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as i8},
                0x30 => JumpIfNotBit { bit: BitAddr::from_byte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?), offset: bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as i8},
                0x40 => JumpIfCarry { offset: bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as i8 },
                0x50 => JumpIfNotCarry { offset: bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as i8 },
                0x60 => JumpIfZero { offset: bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as i8 },
                0x70 => JumpIfNonZero { offset: bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as i8 },
                0x80 => ShortJump { offset: bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as i8 },
                0x90 => MovImmediateDataPtr { value: (bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as u16) | (bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as u16)},
                0xA0 => LogicalOrCarry { src: BitAddr::from_byte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?), complement: true },
                0xB0 => LogicalAndCarry { src: BitAddr::from_byte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?), complement: true },
                0xC0 => Push { addr: bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? },
                0xD0 => Pop { addr: bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? },
                0xE0 => MovXMemToAccDptr,
                0xF0 => MovAccToXMemDptr,
                0x01 | 0x21 | 0x41 | 0x61 | 0x81 | 0xA1 | 0xC1 | 0xE1=> AbsoluteJump { addr: ((byte >> 5) as u16) | (bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as u16)},
                0x11 | 0x31 | 0x51 | 0x71 | 0x91 | 0xB1 | 0xD1 | 0xF1  => AbsoluteCall { addr: ((byte >> 5) as u16) | (bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as u16)},
                0x02 => LongJump { addr: (bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as u16) | (bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as u16) },
                0x12 => LongCall { addr: (bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as u16) | (bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as u16) },
                0x22 => Return,
                0x32 => ReturnFromInterrupt,
                0x72 => LogicalOrCarry { src: BitAddr::from_byte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?), complement: false },
                0x82 => LogicalAndCarry { src: BitAddr::from_byte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?), complement: false },
                0x92 => MovBitCarry { src: BitAddr::from_byte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?) },
                0xA2 => MovCarryBit { dst: BitAddr::from_byte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?) },
                0xB2 => ComplementBit { bit_addr: BitAddr::from_byte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?) },
                0xC2 => ClearBit { bit_addr: BitAddr::from_byte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?) },
                0xD2 => SetBit { bit_addr: BitAddr::from_byte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?) },
                0xE2 => MovXMemToAccIndirect { src: Indirect::R0 },
                0xF2 => MovAccToXMemIndirect { dst: Indirect::R0 },
                0x03 => RotateAccRight,
                0x13 => RotateAccRightThroughCarry,
                0x23 => RotateAccLeft,
                0x33 => RotateAccLeftThroughCarry,
                0x73 => JumpAccPlusDptr,
                0x83 => MovCodeAccPcToAcc,
                0x93 => MovCodeAccDptrToAcc,
                0xA3 => IncrementDPtr,
                0xB3 => ComplementCarry,
                0xC3 => ClearCarry,
                0xD3 => SetCarry,
                0xE3 => MovXMemToAccIndirect { src: Indirect::R1 },
                0xF3 => MovAccToXMemIndirect { dst: Indirect::R1 },
                0x84 => Div,
                0xA4 => Mul,
                0xC4 => SwapAccNibbles,
                0xD4 => DecimalAdjustAcc,
                0xE4 => ClearAcc,
                0xF4 => ComplementAcc,
                0x42 => LogicalOr { src: Operand::SpecialRegister(SpecialFunctionRegister::Accumulator), dst: Operand::MemoryDirect(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?) },
                0x52 => LogicalAnd { src: Operand::SpecialRegister(SpecialFunctionRegister::Accumulator), dst: Operand::MemoryDirect(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?) },
                0x62 => LogicalXor { src: Operand::SpecialRegister(SpecialFunctionRegister::Accumulator), dst: Operand::MemoryDirect(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?) },
                0x43 => LogicalOr { src: Operand::ImmediateByte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?), dst: Operand::MemoryDirect(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?) },
                0x53 => LogicalAnd { src: Operand::ImmediateByte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?), dst: Operand::MemoryDirect(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?) },
                0x63 => LogicalXor { src: Operand::ImmediateByte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?), dst: Operand::MemoryDirect(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?) },
                0x04..=0x0F => Increment {
                    operand: Operand::from_byte::<false, _>(byte, bytes)?,
                },
                0x14..=0x1F => Decrement {
                    operand: Operand::from_byte::<false, _>(byte, bytes)?,
                },
                0x24..=0x2F => Add {
                    src: Operand::from_byte::<true, _>(byte, bytes)?,
                    dst: SpecialRegister(SpecialFunctionRegister::Accumulator),
                    carry: false,
                },
                0x34..=0x3F => Add {
                    src: Operand::from_byte::<true, _>(byte, bytes)?,
                    dst: SpecialRegister(SpecialFunctionRegister::Accumulator),
                    carry: true,
                },
                0x44..=0x4F => LogicalOr {
                    src: Operand::from_byte::<true, _>(byte, bytes)?,
                    dst: Operand::SpecialRegister(SpecialFunctionRegister::Accumulator),
                },
                0x54..=0x5F => LogicalAnd {
                    src: Operand::from_byte::<true, _>(byte, bytes)?,
                    dst: Operand::SpecialRegister(SpecialFunctionRegister::Accumulator),
                },
                0x64..=0x6F => LogicalXor {
                    src: Operand::from_byte::<true, _>(byte, bytes)?,
                    dst: Operand::SpecialRegister(SpecialFunctionRegister::Accumulator),
                },
                0x74..=0x7F => Mov {
                    dst: Operand::from_byte::<false, _>(byte, bytes)?,
                    src: Operand::ImmediateByte(
                        bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?,
                    ),
                },
                0x84..=0x8F => Mov {
                    dst: Operand::MemoryDirect(
                        bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?,
                    ),
                    src: Operand::from_byte::<true, _>(byte, bytes)?,
                },
                0x94..=0x9F => Sub {
                    src: Operand::from_byte::<true, _>(byte, bytes)?,
                    dst: Operand::SpecialRegister(SpecialFunctionRegister::Accumulator),
                },
                0xA4..=0xAF => Mov {
                    src: Operand::MemoryDirect(
                        bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?,
                    ),
                    dst: Operand::from_byte::<false, _>(byte, bytes)?,
                },
                0xB4 | 0xB5 => return Err(InstructionDecodeError::InvalidByte),
                0xB6..=0xBF => CompareJumpNotEqual {
                    src: Operand::from_byte::<true, _>(byte, bytes).expect(&format!("This match arm should only be reached with +0 operands (low nibble 0x6..0xF) got {:x}", byte&0xf)),
                    compared_to: Operand::ImmediateByte(bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)?),
                    offset: bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as i8,
                },
                0xC5..=0xCF => Exchange { dst: Operand::from_byte::<false, _>(byte, bytes)? },
                0xD4..=0xDF => DecrementJumpNotZero { operand: Operand::from_byte::<false, _>(byte, bytes)?, offset: bytes.next().ok_or(InstructionDecodeError::NotEnoughBytes)? as i8},
                0xE5..=0xEF => Mov { src: Operand::from_byte::<true, _>(byte, bytes)?, dst: Operand::SpecialRegister(SpecialFunctionRegister::Accumulator) },
                0xF5..=0xFF => Mov { src: Operand::SpecialRegister(SpecialFunctionRegister::Accumulator), dst: Operand::from_byte::<false, _>(byte, bytes)?},
            }
        })
    }

    fn decode_stream<ByteStream: IntoIterator<Item = u8>>(
        stream: ByteStream,
        size_hint: Option<usize>,
    ) -> Result<Vec<Instruction>, InstructionDecodeError> {
        let mut buffer = Vec::with_capacity(size_hint.unwrap_or(1024));
        let mut iter = stream.into_iter();
        loop {
            match Self::decode_one(&mut iter) {
                Ok(instr) => buffer.push(instr),
                Err(InstructionDecodeError::CompletelyExhausted) => break,
                Err(err) => return Err(err),
            }
        }

        if buffer.capacity() > 2 * buffer.len() {
            buffer.shrink_to_fit();
        }

        return Ok(buffer);
    }
}
