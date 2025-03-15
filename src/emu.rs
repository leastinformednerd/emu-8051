use std::default;

use bitflags::bitflags;

use crate::isa;

type SixtyFourKBMem = [u8; u16::MAX as usize];

bitflags! {
    #[derive(Debug, Default, Clone, Copy)]
    struct PowerControl: u8 {
        const Idle = 1;
        const PowerDown = 2;
        const GeneralFlag0 = 4;
        const GeneralFlag1 = 8;
        const SerialMode = 128;
    }

    #[derive(Debug, Default, Clone, Copy)]
    struct TimerControl: u8 {
        const ExternalInterrupt0Control = 1;
        const ExternalInterrupt0Called = 2;
        const ExternalInterrupt1Control = 4;
        const ExternalInterrupt1Called = 8;
        const Timer0Enabled = 16;
        const Timer0OverflowFlag = 32;
        const Timer1Enabled = 64;
        const Timer1OverflowFlag = 128;
    }

    #[derive(Debug, Default, Clone, Copy)]
    struct TimerMode: u8 {
        const T0ModeSelector0 = 1;
        const T0ModeSelector1 = 2;
        const T0TimerCounter = 4;
        const T0Gate = 8;
        const T1ModeSelector0 = 16;
        const T1ModeSelector1 = 32;
        const T1TimerCounter = 64;
        const T1Gate = 128;
    }

    #[derive(Debug, Default, Clone, Copy)]
    struct SerialControl: u8 {
        const RxdFlag = 1;
        const TxdFlag = 2;
        const RxBit8 = 4;
        const TxBit8 = 8;
        const ReceptionEnable = 16;
        const FeatureMode = 32;
        const SerialMode1 = 64;
        const SerialMode0 = 128;
    }

    #[derive(Debug, Default, Clone, Copy)]
    struct InterruptEnable: u8 {
        const EnableExternal0 = 1;
        const EnableTimer0Overflow = 2;
        const EnableExternal1 = 4;
        const EnableTimer1Overflow = 8;
        const EnableSerialDone = 16;
        const EnableInterrupts = 128;
    }

    #[derive(Debug, Default, Clone, Copy)]
    struct InterruptPriority: u8 {
        const PrioritiseExternal0 = 1;
        const PrioritiseTimer0Overflow = 2;
        const PrioritiseExternal1 = 4;
        const PrioritiseTimer1Overflow = 8;
        const PrioritiseSerialDone = 16;
    }
}

#[derive(Debug, Default)]
struct SpecialFunctionRegisters {
    accumulator: u8,
    accumulator_b: u8,
    program_status: u8,
    stack_pointer: u8,
    data_pointer: u16,
    power_control: PowerControl,
    timer_control: TimerControl,
    timer_mode: TimerMode,
    timer0: u16,
    timer1: u16,
    pin0: bool,
    pin1: bool,
    pin2: bool,
    pin3: bool,
    serial_buffer: u8,
    serial_control: SerialControl,
    interrupt_enable: InterruptEnable,
    interrupt_priority: InterruptPriority,
}

impl SpecialFunctionRegisters {
    fn get(&self, reg: isa::SpecialFunctionRegister) -> u8 {
        use isa::SpecialFunctionRegister::*;
        match reg {
            Accumulator => self.accumulator,
            AccExtension => self.accumulator_b,
            ProgramStatus => self.program_status,
            StackPointer => self.stack_pointer,
            DPtrLow => self.data_pointer as u8,
            DPtrHigh => (self.data_pointer >> 8) as u8,
            PowerControl => self.power_control.bits(),
            TimerControl => self.timer_control.bits(),
            TimerMode => self.timer_mode.bits(),
            Timer0Low => self.timer0 as u8,
            Timer0High => (self.timer0 >> 8) as u8,
            Timer1Low => self.timer1 as u8,
            Timer1High => (self.timer1 >> 8) as u8,
            IOPort0 => self.pin0 as u8,
            IOPort1 => self.pin1 as u8,
            IOPort2 => self.pin2 as u8,
            IOPort3 => self.pin3 as u8,
            SerialControl => self.serial_control.bits(),
            Buffer => self.serial_buffer,
            InterruptEnable => self.interrupt_enable.bits(),
            InterruptPriority => self.interrupt_priority.bits(),
        }
    }
}

impl SpecialFunctionRegisters {
    fn new() -> Self {
        let mut s: Self = default::Default::default();
        s.stack_pointer = 0x07;
        s
    }
}

#[derive(Debug)]
struct EmulatorState {
    internal_ram: [u8; 256],
    special_registers: SpecialFunctionRegisters,
    program_memory: SixtyFourKBMem,
    external_memory: SixtyFourKBMem,
}

impl EmulatorState {
    fn new() -> Self {
        EmulatorState {
            internal_ram: [0; 256],
            special_registers: SpecialFunctionRegisters::new(),
            program_memory: [0; u16::MAX as usize],
            external_memory: [0; u16::MAX as usize],
        }
    }

    fn get_general_reg(&self, reg: isa::GeneralPurposeRegister) -> u8 {
        let offset = reg.num();
        let reg_select_0 = (self.special_registers.program_status & 0b1000) >> 3;
        let reg_select_1 = (self.special_registers.program_status & 0b10000) >> 4;
        let window = reg_select_0 * 0x8 + reg_select_1 * 0x10;

        self.internal_ram[(window + offset) as usize]
    }
}

#[derive(Debug)]
pub struct Message {
    /// If Some tries writing a byte in the processor's serial buffer
    pub write_serial: Option<u8>,

    /// If Some tries writing a value into each of the I/O pins
    pub io_pins: Option<(u8, u8, u8)>,

    /// If true triggers External Interrupt 0
    pub external_interrupt_0: bool,

    /// If true triggers External Interrupt 0
    pub external_interrupt_1: bool,
}

/// A message to pass the emulator about the outside state or control information
#[derive(Debug)]
pub enum InputMessage {
    /// Passes a message to the emulator descring a change in external state
    /// Also continues execution
    Message(Message),

    /// Debug return a reference of internal memory
    DebugDumpAllIMem,
    DebugDumpIMemSlice(std::ops::Range<usize>),
    DebugSpecialRegister(isa::SpecialFunctionRegister),
    DebugGeneralRegister(isa::GeneralPurposeRegister),
    DebugDumpAllExMem,
    DebugDumpExMemSlice(std::ops::Range<usize>),

    /// Continue execution without changing external state / doing I/O
    None,
}

enum ProcessorOutput<'a> {
    Normal {
        serial_out: Option<u8>,
        io_pin_out: Option<(u8, u8, u8)>,
    },
    DebugMem(&'a [u8]),
    DebugRegisterByte(u8),
    InvalidMessage,
}

fn process_timer(
    timer: &mut u16,
    timer_no: u8,
    timer_control: &mut TimerControl,
    timer_mode: TimerMode,
) {
    let (mode0, mode1) = match timer_no {
        0 => (
            timer_mode.contains(TimerMode::T0ModeSelector0),
            timer_mode.contains(TimerMode::T0ModeSelector1),
        ),
        1 => (
            timer_mode.contains(TimerMode::T1ModeSelector0),
            timer_mode.contains(TimerMode::T1ModeSelector1),
        ),
        _ => panic!("Timer{timer_no} is not supported"),
    };

    let mode = ((mode1 as u8) << 1) | (mode0 as u8);

    match mode {
        0 => {
            *timer += 1;
            if *timer >= 2u16 << 13 {
                *timer = 0;
                timer_control.insert(if timer_no == 0 {
                    TimerControl::Timer0OverflowFlag
                } else {
                    TimerControl::Timer1OverflowFlag
                })
            }
        }
        1 => (*timer, _) = timer.overflowing_add(1),
        2 => {
            if let (_, true) = ((*timer >> 8) as u8).overflowing_add(1) {
                *timer = (*timer & 0xF) | ((*timer & 0xF) << 8)
            } else {
                // I'm sure there are better ways to do this lmao
                *timer = (((((*timer >> 8) as u8) + 1) as u16) << 8) | (*timer & 0xF)
            }
        }
        3 if timer_no == 0 => {
            let mut fake0 = (*timer & 0xFF) << (8 * i);
            process_timer(&mut fake0, 0, timer_control, timer_mode);
            let mut fake1 = (*timer & 0xFF) << (8 * i);
            process_timer(&mut fake1, 1, timer_control, timer_mode);

            *timer = u16::from_be_bytes([(fake0 & 0xF) as u8, (fake1 & 0xF) as u8])
        }
        3 if timer_no == 1 => {}
        _ => unreachable!(),
    }
}

#[derive(Debug)]
struct Emulator {
    state: EmulatorState,
    pin_cache: (u8, u8, u8),
    uses_exmem: bool,
    oscillator_tick: u64,
}

impl Emulator {
    pub fn new(uses_exmem: bool) -> Self {
        Emulator {
            state: EmulatorState::new(),
            pin_cache: (0, 0, 0),
            oscillator_tick: 0,
            uses_exmem,
        }
    }

    fn step(&mut self, msg: InputMessage) -> ProcessorOutput {
        match msg {
            InputMessage::Message(Message { .. }) => {
                todo!();
                self.handle_tick()
            }
            InputMessage::DebugDumpAllIMem => ProcessorOutput::DebugMem(&self.state.internal_ram),
            InputMessage::DebugDumpIMemSlice(range) => {
                ProcessorOutput::DebugMem(&self.state.internal_ram[range])
            }
            InputMessage::DebugSpecialRegister(reg) => {
                ProcessorOutput::DebugRegisterByte(self.state.special_registers.get(reg))
            }
            InputMessage::DebugGeneralRegister(reg) => {
                ProcessorOutput::DebugRegisterByte(self.state.get_general_reg(reg))
            }
            InputMessage::DebugDumpAllExMem => {
                if self.uses_exmem {
                    ProcessorOutput::DebugMem(&self.state.external_memory)
                } else {
                    ProcessorOutput::InvalidMessage
                }
            }
            InputMessage::DebugDumpExMemSlice(range) => {
                if self.uses_exmem {
                    ProcessorOutput::DebugMem(&self.state.external_memory[range])
                } else {
                    ProcessorOutput::InvalidMessage
                }
            }
            InputMessage::None => self.handle_tick(),
        }
    }

    fn handle_tick(&mut self) -> ProcessorOutput {
        self.oscillator_tick += 1;

        process_timer(
            &mut self.state.special_registers.timer0,
            0,
            &mut self.state.special_registers.timer_control,
            self.state.special_registers.timer_mode,
        );
        process_timer(
            &mut self.state.special_registers.timer1,
            1,
            &mut self.state.special_registers.timer_control,
            self.state.special_registers.timer_mode,
        );

        todo!()
    }
}
