
use std::num::Wrapping;
use std::fmt;

use {Memory, Error, Symbols};
use self::disasm::{Disasm, DisasmMode};

mod disasm;
mod mmu;

pub struct Hobbit {
    /// the program counter
    pub pc: Wrapping<u32>,
    // a shadow of the pc
    instruction_pc: Wrapping<u32>,
    /// the stack pointer
    pub sp: Wrapping<u32>,
    /// the interrupt sp
    pub isp: Wrapping<u32>,
    /// the maximum stack pointer
    pub msp: Wrapping<u32>,
    ///
    pub program_status_word: Wrapping<u32>,
    ///
    pub timer: [Wrapping<u32>; 2],
    ///
    pub config: Wrapping<u32>,
    ///
    pub vector_base: Wrapping<u32>,

    //
    pub mmu: mmu::MMU,

    /// the cpu_mode_escape happens after
    /// decoding an instruction. chaning the meaning
    pub cpu_mode_escape: bool,

    // callstack depth
    pub depth: usize,

    pub pending_interrupts: u32,
}

impl fmt::Display for Hobbit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, " pc: {:08x}", self.pc)?;
        writeln!(f, " sp: {:08x}", self.sp)?;
        writeln!(f, " isp: {:08x}", self.isp)?;
        Ok(())
    }
}

impl Hobbit {
    pub fn new() -> Hobbit {
        Hobbit {
            pc: Wrapping(0),
            instruction_pc: Wrapping(0),
            // TODO, figure out of this is right
            sp: Wrapping(0x810_0000),
            isp: Wrapping(0x810_0000),
            msp: Wrapping(0),
            program_status_word: Wrapping(0),
            timer: [Wrapping(0); 2],
            config: Wrapping(0x40000),
            vector_base: Wrapping(0),
            mmu: mmu::MMU::new(),

            cpu_mode_escape: false,
            depth: 0,
            pending_interrupts: 0,
        }
    }

    pub fn load<M: Memory>(&mut self, memory: &mut M, src: Mode) -> Result<Wrapping<u32>, Error> {
        let mut memory = (&mut self.mmu, memory);

        match src {
            Mode::CpuRegister(2) => Ok(self.isp),
            Mode::CpuRegister(4) => Ok(self.config),
            Mode::CpuRegister(5) => Ok(self.program_status_word),
            Mode::CpuRegister(7) => Ok(self.vector_base),
            Mode::CpuRegister(10) => Ok(Wrapping(0xe33d03b)),
            Mode::CpuRegister(11) => Ok(self.timer[0]),
            Mode::CpuRegister(12) => Ok(self.timer[1]),
            Mode::Immediate(x) => Ok(Wrapping(x)),

            Mode::StackOffsetUnsignedByte(off) => {
                let val = memory.read_u8((self.sp + Wrapping(off)).0)?;
                Ok(Wrapping(val as u32))
            }
            Mode::StackOffsetByte(off) => {
                let val = memory.read_i8((self.sp + Wrapping(off)).0)?;
                Ok(Wrapping(val.extend()))
            }
            Mode::StackOffsetUnsignedHalfword(off) => {
                let val = memory.read_u16((self.sp + Wrapping(off)).0)?;
                Ok(Wrapping(val as u32))
            } 
            Mode::StackOffsetHalfword(off) => {
                let val = memory.read_i16((self.sp + Wrapping(off)).0)?;
                Ok(Wrapping(val.extend()))
            }
            Mode::StackOffsetWord(off) => {
                let val = memory.read_u32((self.sp + Wrapping(off)).0)?;
                Ok(Wrapping(val as u32))
            }

            Mode::AbsoluteByte(addr, _) => Ok(Wrapping(memory.read_i8(addr)?.extend())),
            Mode::AbsoluteUnsignedByte(addr, _) => Ok(Wrapping(memory.read_u8(addr)? as u32)),
            Mode::AbsoluteUnsignedHalfword(addr, _) => {
                Ok(Wrapping(memory.read_u16(addr)? as u32))
            }
            Mode::AbsoluteHalfword(addr, _) => Ok(Wrapping(memory.read_i16(addr)?.extend())),
            Mode::AbsoluteWord(addr, _) => Ok(Wrapping(memory.read_u32(addr)? as u32)),

            Mode::StackOffsetIndirectByte(off) => {
                let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                Ok(Wrapping(memory.read_i8(addr)?.extend()))
            }
            Mode::StackOffsetIndirectUnsignedByte(off) => {
                let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                Ok(Wrapping(memory.read_u8(addr)? as u32))
            }
            Mode::StackOffsetIndirectHalfword(off) => {
                let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                Ok(Wrapping(memory.read_i16(addr)?.extend()))
            }
            Mode::StackOffsetIndirectUnsignedHalfword(off) => {
                let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                Ok(Wrapping(memory.read_u16(addr)? as u32))
            }
            Mode::StackOffsetIndirectWord(off) => {
                let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                Ok(Wrapping(memory.read_u32(addr)?))
            }

            _ => panic!("unhandled smode {:?}", src),
        }
    }

    pub fn load_flow<M>(&mut self, memory: &mut M, src: Mode) -> Result<Wrapping<u32>, Error>
        where M: Memory
    {
        let mut memory = (&mut self.mmu, memory);

        Ok(match src {
               Mode::AbsoluteWord(addr, _) => Wrapping(memory.read_u32(addr)? as u32),
               Mode::Immediate(addr) => Wrapping(addr),
               Mode::StackOffsetWord(off) => {
                   Wrapping(memory.read_u32((self.sp + Wrapping(off)).0)?)
               }
               Mode::PCRelative(x) => self.instruction_pc + Wrapping(x),
               _ => panic!("unhandled smode {:?}", src),
           })
    }

    pub fn store<M: Memory>(&mut self,
                            memory: &mut M,
                            dst: Mode,
                            val: Wrapping<u32>)
                            -> Result<(), Error> {


        match dst {
            Mode::CpuRegister(2) => {
                self.isp = val;
            }
            Mode::CpuRegister(4) => {
                self.config = val;
            }
            Mode::CpuRegister(5) => {
                // forward the VP fpag to the mmu
                self.mmu.enabled = val.0 & 0x1_0000 == 0x1_0000;
                // forward the user mode to the mmu
                self.mmu.user = val.0 & (1 << 10) == (1 << 10);
                self.program_status_word = val;
            }
            Mode::CpuRegister(7) => {
                self.vector_base = val;
            }
            Mode::CpuRegister(8) => {
                self.mmu.segment_table_base = val.0;
            }
            Mode::CpuRegister(11) => {
                self.timer[0] = val;
            }
            Mode::CpuRegister(12) => {
                self.timer[1] = val;
            }

            Mode::StackOffsetByte(off) => {
                let mut memory = (&mut self.mmu, memory);
                memory.write_i8((self.sp + Wrapping(off)).0, val.0 as i8)?;
            }
            Mode::StackOffsetUnsignedByte(off) => {
                let mut memory = (&mut self.mmu, memory);
                memory.write_u8((self.sp + Wrapping(off)).0, val.0 as u8)?;
            }
            Mode::StackOffsetHalfword(off) => {
                let mut memory = (&mut self.mmu, memory);
                memory.write_i16((self.sp + Wrapping(off)).0, val.0 as i16)?;
            }
            Mode::StackOffsetUnsignedHalfword(off) => {
                let mut memory = (&mut self.mmu, memory);
                memory.write_u16((self.sp + Wrapping(off)).0, val.0 as u16)?;
            }
            Mode::StackOffsetWord(off) => {
                let mut memory = (&mut self.mmu, memory);
                memory.write_u32((self.sp + Wrapping(off)).0, val.0)?;
            }
            Mode::StackOffsetIndirectByte(off) => {
                let mut memory = (&mut self.mmu, memory);
                let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                memory.write_i8(addr, val.0 as i8)?;
            }
            Mode::StackOffsetIndirectUnsignedByte(off) => {
                let mut memory = (&mut self.mmu, memory);
                let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                memory.write_u8(addr, val.0 as u8)?;
            }
            Mode::StackOffsetIndirectHalfword(off) => {
                let mut memory = (&mut self.mmu, memory);
                let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                memory.write_i16(addr, val.0 as i16)?;
            }
            Mode::StackOffsetIndirectUnsignedHalfword(off) => {
                let mut memory = (&mut self.mmu, memory);
                let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                memory.write_u16(addr, val.0 as u16)?;
            }
            Mode::StackOffsetIndirectWord(off) => {
                let mut memory = (&mut self.mmu, memory);
                let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                memory.write_u32(addr, val.0)?;
            }
            Mode::AbsoluteByte(addr, false) => {
                let mut memory = (&mut self.mmu, memory);
                memory.write_i8(addr, val.0 as i8)?;
            }
            Mode::AbsoluteUnsignedByte(addr, false) => {
                let mut memory = (&mut self.mmu, memory);
                memory.write_u8(addr, val.0 as u8)?;
            }
            Mode::AbsoluteHalfword(addr, false) => {
                let mut memory = (&mut self.mmu, memory);
                memory.write_i16(addr, val.0 as i16)?;
            }
            Mode::AbsoluteUnsignedHalfword(addr, false) => {
                let mut memory = (&mut self.mmu, memory);
                memory.write_u16(addr, val.0 as u16)?;
            }
            Mode::AbsoluteWord(addr, false) => {
                let mut memory = (&mut self.mmu, memory);
                memory.write_u32(addr, val.0)?;
            }
            _ => panic!("unhandled dmode {:?}", dst),
        }
        Ok(())
    }

    #[allow(dead_code)]
    pub fn walk<M: Memory>(&self,
                           memory: &mut M,
                           start: u32,
                           stop: u32,
                           symbols: Option<Symbols>) {
        let mut addr = start;
        while addr < stop {
            match Instruction::decode(memory, addr, self.cpu_mode_escape) {
                Ok((instruction, next)) => {
                    if instruction.is_invalid() {
                        println!("invalid {:?}", instruction);
                        return;
                    }
                    println!("\t{:08x}\t{}",
                             addr,
                             Disasm(instruction, symbols, Some(addr)));
                    addr += next;
                }
                Err(_) => return,
            }
        }
    }

    pub fn execute<'a, M>(&mut self, instruction: Instruction, memory: &mut M) -> Result<(), Error>
        where M: Memory
    {
        match instruction {
            Instruction::Dyadic(op, src, dst) => {
                let a = self.load(memory, src)?;
                let b = self.load(memory, dst)?;
                let res = op.eval(a, b);
                self.store(memory, dst, res)?;
            }

            Instruction::Dyadic3(op, src, dst) => {
                let a = self.load(memory, src)?;
                let b = self.load(memory, dst)?;
                let res = op.eval(a, b);
                self.store(memory, Mode::StackOffsetWord(4), res)?;
            }

            Instruction::DyadicI(op, src, dst) => {
                let a = self.load(memory, src)?;
                let b = self.load(memory, dst)?;
                let res = op.eval(a, b);
                self.store(memory, dst, res)?;
                self.store(memory, Mode::StackOffsetWord(4), b)?;
            }

            Instruction::CPU => {
                self.cpu_mode_escape = true;
            }
            Instruction::Cret => {
                let mut memory = (&mut self.mmu, memory);
                self.sp = Wrapping(memory.read_u32(self.isp.0)?);
                self.pc = Wrapping(memory.read_u32((self.isp + Wrapping(8)).0)?);
                self.isp += Wrapping(16);
                if self.depth > 0 {
                    self.depth -= 1;
                }
            }
            Instruction::Flushi => (),
            Instruction::Flushp => (),
            Instruction::Kret => {
                let mut memory = (&mut self.mmu, memory);
                self.pc = Wrapping(memory.read_u32((self.isp + Wrapping(8)).0)?);
                self.isp += Wrapping(16);
            }
            Instruction::Mov(src, dst) => {
                let val = self.load(memory, src)?;
                self.store(memory, dst, val)?;
            }
            Instruction::Mova(src, dst) => {
                let val = match src {
                    Mode::StackOffsetByte(x) |
                    Mode::StackOffsetUnsignedByte(x) |
                    Mode::StackOffsetHalfword(x) |
                    Mode::StackOffsetUnsignedHalfword(x) |
                    Mode::StackOffsetWord(x) => (self.sp + Wrapping(x)).0,

                    Mode::AbsoluteByte(x, _) |
                    Mode::AbsoluteUnsignedByte(x, _) |
                    Mode::AbsoluteHalfword(x, _) |
                    Mode::AbsoluteUnsignedHalfword(x, _) |
                    Mode::AbsoluteWord(x, _) => x,

                    _ => panic!("unimplemented"),
                };
                self.store(memory, dst, Wrapping(val))?;
            }
            Instruction::Nop => (),
            Instruction::Catch(_) => {}
            Instruction::Call(mode) => {
                let addr = self.load_flow(memory, mode)?;
                memory.write_u32(self.sp.0, self.pc.0)?;
                self.pc = addr;
                self.depth += 1;
            }
            Instruction::Enter(Mode::StackOffsetWord(off)) => {
                self.sp += Wrapping(off);
            }
            Instruction::Jmp(mode) => {
                let addr = self.load_flow(memory, mode)?;
                self.pc = addr;
            }
            Instruction::JmpIf(mode, if_true, _) => {
                let addr = self.load_flow(memory, mode)?;
                let is_true = self.program_status_word & Wrapping(0x10) != Wrapping(0);
                if is_true == if_true {
                    self.pc = addr;
                }
            }
            Instruction::Ldraa(mode) => {
                let addr = self.load_flow(memory, mode)?;
                self.store(memory, Mode::StackOffsetWord(4), addr)?;
            }
            Instruction::Return(Mode::StackOffsetWord(off)) => {
                let ret = self.load(memory, Mode::StackOffsetWord(off))?;
                self.sp += Wrapping(off);
                self.pc = ret;
                if self.depth > 0 {
                    self.depth -= 1;
                }
            }
            Instruction::Cmp(op, src, dst) => {
                let src = self.load(memory, src)?;
                let dst = self.load(memory, dst)?;
                if op.eval(src, dst) {
                    self.program_status_word |= Wrapping(0x10);
                } else {
                    self.program_status_word &= Wrapping(!0x10);
                }
            }
            Instruction::Dqm(src, dst) => {
                let mut memory = (&mut self.mmu, memory);

                let (addr, cnt): (u32, u32) = match dst {
                    Mode::AbsoluteByte(addr, _) => (addr, 2),
                    Mode::AbsoluteWord(addr, _) => (addr, 4),
                    Mode::StackOffsetByte(off) => (self.sp.0 + off, 2),
                    Mode::StackOffsetWord(off) => (self.sp.0 + off, 4),
                    Mode::StackOffsetIndirectByte(addr) => {
                        let addr = memory.read_u32(self.sp.0 + addr)?;
                        (addr, 2)
                    }
                    Mode::StackOffsetIndirectWord(addr) => {
                        let addr = memory.read_u32(self.sp.0 + addr)?;
                        (addr, 4)
                    }
                    mode => panic!("illigal mode for dqm dst: {:?}", mode),
                };

                for i in 0..cnt {
                    let val = match src {
                        Mode::AbsoluteWord(a, _) => memory.read_u32(a + i * 4)?,
                        Mode::StackOffsetWord(off) => {
                            memory.read_u32(self.sp.0 + off + i * 4)?
                        }
                        Mode::StackOffsetIndirectWord(off) => {
                            let addr = memory.read_u32(self.sp.0 + off)?;
                            memory.read_u32(addr + i * 4)?
                        }
                        Mode::Immediate(x) => x,
                        mode => panic!("illigal mode for dqm src: {:?}", mode),
                    };

                    memory.write_u32(addr + i * 4, val)?;
                }
            }
            Instruction::Kcall(src) => {
                let src = self.load(memory, src)?;
                {
                    let mut memory = (&mut self.mmu, memory);
                    memory.write_u32(self.isp.0 - 12, src.0)?;
                    memory.write_u32(self.isp.0 - 8, self.pc.0)?;
                    memory.write_u32(self.isp.0 - 4, self.program_status_word.0)?;
                    self.isp -= Wrapping(0);
                    self.pc = Wrapping(memory.read_u32(self.vector_base.0)?);
                }
                self.mmu.user = false;
                self.program_status_word &= Wrapping(0xffff_0000);
            }
            x => {
                panic!("unhandled instruction {:?}", x);
            }
        }
        Ok(())
    }

    #[allow(dead_code)]
    pub fn explain_load<M: Memory>(&mut self,
                                   memory: &mut M,
                                   src: Mode)
                                   -> Result<(Wrapping<u32>, String), Error> {
        match src {
            Mode::CpuRegister(2) => Ok((self.isp, format!("$isp<{:x}>", self.isp))),
            Mode::CpuRegister(4) => Ok((self.config, format!("$config<{:x}>", self.config))),
            Mode::CpuRegister(5) => {
                Ok((self.program_status_word, format!("$psw<{:x}>", self.program_status_word)))
            }
            Mode::CpuRegister(7) => Ok((self.vector_base, format!("$vb<{:x}>", self.vector_base))),
            Mode::CpuRegister(10) => Ok((Wrapping(0xe33d03b), format!("$id<{:x}>", 0xe33d03b))),
            Mode::CpuRegister(11) => Ok((self.timer[0], format!("$timer1<{:x}>", self.timer[0]))),
            Mode::CpuRegister(12) => Ok((self.timer[1], format!("$timer2<{:x}>", self.timer[1]))),
            Mode::Immediate(x) => Ok((Wrapping(x), format!("${:x}", x))),

            Mode::StackOffsetUnsignedByte(off) => {
                let val = memory.read_u8((self.sp + Wrapping(off)).0)?;
                let s = format!("R{}<{:x}>:ub", off, val);
                Ok((Wrapping(val as u32), s))
            }
            Mode::StackOffsetByte(off) => {
                let val = memory.read_i8((self.sp + Wrapping(off)).0)?;
                let s = format!("R{}<{:x}>:b", off, val);
                Ok((Wrapping(val.extend()), s))
            }
            Mode::StackOffsetUnsignedHalfword(off) => {
                let val = memory.read_u16((self.sp + Wrapping(off)).0)?;
                let s = format!("R{}<{:x}>:uh", off, val);
                Ok((Wrapping(val as u32), s))
            } 
            Mode::StackOffsetHalfword(off) => {
                let val = memory.read_i16((self.sp + Wrapping(off)).0)?;
                let s = format!("R{}<{:x}>:h", off, val);
                Ok((Wrapping(val.extend()), s))
            }
            Mode::StackOffsetWord(off) => {
                let val = memory.read_u32((self.sp + Wrapping(off)).0)?;
                let s = format!("R{}<{:x}>", off, val);
                Ok((Wrapping(val), s))
            }

            Mode::AbsoluteByte(addr, _) => {
                let val = Wrapping(memory.read_i8(addr)?.extend());
                let s = format!("(*${:x}:b => {:x})", addr, val);
                Ok((val, s))
            }
            Mode::AbsoluteUnsignedByte(addr, _) => {
                let val = Wrapping(memory.read_u8(addr)? as u32);
                let s = format!("(*${:x}:ub => {:x})", addr, val);
                Ok((val, s))
            }
            Mode::AbsoluteUnsignedHalfword(addr, _) => {
                let val = Wrapping(memory.read_u16(addr)? as u32);
                let s = format!("(*${:x}:uh => {:x})", addr, val);
                Ok((val, s))
            }
            Mode::AbsoluteHalfword(addr, _) => {
                let val = Wrapping(memory.read_i16(addr)?.extend());
                let s = format!("(*${:x}:h => {:x})", addr, val);
                Ok((val, s))
            }
            Mode::AbsoluteWord(addr, _) => {
                let val = Wrapping(memory.read_u32(addr)? as u32);
                let s = format!("(*${:x} => {:x})", addr, val);
                Ok((val, s))
            }

            Mode::StackOffsetIndirectByte(off) => {
                let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                let res = Wrapping(memory.read_i8(addr)?.extend());
                let s = format!("(*R{}<{:x}>:b => {:x})", off, addr, res);
                Ok((res, s))

            }
            Mode::StackOffsetIndirectUnsignedByte(off) => {
                let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                let res = Wrapping(memory.read_u8(addr)? as u32);
                let s = format!("(*R{}<{:x}>:ub => {:x})", off, addr, res);
                Ok((res, s))

            }
            Mode::StackOffsetIndirectHalfword(off) => {
                let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                let res = Wrapping(memory.read_i16(addr)?.extend());
                let s = format!("(*R{}<{:x}>:h => {:x})", off, addr, res);
                Ok((res, s))

            }
            Mode::StackOffsetIndirectUnsignedHalfword(off) => {
                let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                let res = Wrapping(memory.read_u16(addr)? as u32);
                let s = format!("(*R{}<{:x}>:uh => {:x})", off, addr, res);
                Ok((res, s))

            }
            Mode::StackOffsetIndirectWord(off) => {
                let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                let res = Wrapping(memory.read_u32(addr)?);
                let s = format!("(*R{}<{:x}> => {:x})", off, addr, res);
                Ok((res, s))

            }

            _ => panic!("unhandled smode {:?}", src),
        }
    }

    #[allow(dead_code)]
    pub fn explain_store<M: Memory>(&mut self,
                                    memory: &mut M,
                                    src: Mode,
                                    val: u32)
                                    -> Result<String, Error> {
        Ok(match src {
               Mode::CpuRegister(2) => format!("$isp<{:x}>", val),
               Mode::CpuRegister(4) => format!("$config<{:x}>", val),
               Mode::CpuRegister(5) => format!("$psw<{:x}>", val),
               Mode::CpuRegister(7) => format!("$vb<{:x}>", val),
               Mode::CpuRegister(8) => format!("$stb<{:x}>", val),
               Mode::CpuRegister(11) => format!("$timer1<{:x}>", val),
               Mode::CpuRegister(12) => format!("$timer2<{:x}>", val),

               Mode::StackOffsetUnsignedByte(off) => format!("R{}<{:x}>:ub", off, val),
               Mode::StackOffsetByte(off) => format!("R{}<{:x}>:b", off, val),
               Mode::StackOffsetUnsignedHalfword(off) => format!("R{}<{:x}>:uh", off, val), 
               Mode::StackOffsetHalfword(off) => format!("R{}<{:x}>:h", off, val),
               Mode::StackOffsetWord(off) => format!("R{}<{:x}>", off, val),
               Mode::AbsoluteByte(addr, _) => format!("(*${:x}:b)<{:x}>", addr, val),
               Mode::AbsoluteUnsignedByte(addr, _) => format!("(*${:x}:ub)<{:x}>", addr, val),
               Mode::AbsoluteUnsignedHalfword(addr, _) => format!("(*${:x}:uh)<{:x}>", addr, val),
               Mode::AbsoluteHalfword(addr, _) => format!("(*${:x}:h)<{:x}>", addr, val),
               Mode::AbsoluteWord(addr, _) => format!("(*${:x})<{:x}>", addr, val),
               Mode::StackOffsetIndirectByte(off) => {
                   let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                   format!("(*R{}<{:x}>:b)<{:x}>", off, addr, val)
               }
               Mode::StackOffsetIndirectUnsignedByte(off) => {
                   let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                   format!("(*R{}<{:x}>:ub)<{:x}>", off, addr, val)
               }
               Mode::StackOffsetIndirectHalfword(off) => {
                   let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                   format!("(*R{}<{:x}>:h)<{:x}>", off, addr, val)
               }
               Mode::StackOffsetIndirectUnsignedHalfword(off) => {
                   let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                   format!("(*R{}<{:x}>:uh)<{:x}>", off, addr, val)
               }
               Mode::StackOffsetIndirectWord(off) => {
                   let addr = memory.read_u32((self.sp + Wrapping(off)).0)?;
                   format!("(*R{}<{:x}>)<{:x}>", off, addr, val)
               }
               _ => panic!("unhandled smode {:?}", src),
           })
    }

    #[allow(dead_code)]
    pub fn explain<'a, M>(&mut self,
                          instruction: Instruction,
                          memory: &mut M,
                          symbols: Option<Symbols<'a>>)
                          -> Result<(), Error>
        where M: Memory
    {
        match instruction {
            Instruction::Dyadic(op, src, dst) => {
                let (a, ea) = self.explain_load(memory, src)?;
                let (b, eb) = self.explain_load(memory, dst)?;
                let res = self.explain_store(memory, dst, op.eval(a, b).0)?;
//                println!("{} {} {} => {}", eb, op.symbol(), ea, res);
            }

            Instruction::Dyadic3(op, src, dst) => {
                let (a, ea) = self.explain_load(memory, src)?;
                let (b, eb) = self.explain_load(memory, dst)?;
                let res =
                    self.explain_store(memory, Mode::StackOffsetWord(4), op.eval(a, b).0)?;
//                println!("{} {} {} => {}", eb, op.symbol(), ea, res);

            }
            Instruction::Call(mode) => {
                let addr = self.load_flow(memory, mode)?;
/*
                println!("R0 = $pc, $pc = {0}; {1:x} = {1:x}, {2:x} = {2};",
                         DisasmMode(mode, symbols, Some(self.instruction_pc.0)),
                         self.pc,
                         addr);
*/
                for i in 0..8 {
                    let i = i * 4;
//                    println!("R{} = 0x{:08x}", i, memory.read_u32(self.sp.0 + i).unwrap());
                }
            }

            Instruction::Cmp(op, src, dst) => {
                let (a, ea) = self.explain_load(memory, src)?;
                let (b, eb) = self.explain_load(memory, dst)?;
                let res = op.eval(a, b);
//                println!("{} {} {} => psw.f<{}>", eb, op.symbol(), ea, res);
            }

            Instruction::Mov(src, dst) => {
                let (val, eval) = self.explain_load(memory, src)?;
                let res = self.explain_store(memory, dst, val.0)?;
//                println!("{} => {}", eval, res);
            }

            _ => (), // println!(""),
// patterns `Instruction::CPU`, `Instruction::Cret`, `Instruction::Flushi` and 16 more not covered
        }
        Ok(())
    }


    fn step_timers(&mut self) {
        let t1_config = (self.config.0 >> 22) & 0x7;

        let old = self.timer[0];
        self.timer[0] += Wrapping(1);

        if t1_config & 0b100 == 0b100 {

            if old > self.timer[0] {
                self.pending_interrupts |= 1 << 7;
                //panic!("{:?}", self.pending_interrupts);
            }
        }
    }

    #[allow(dead_code)]
    fn check_interrupts<M: Memory>(&mut self, memory: &mut M) {
        if self.cpu_mode_escape {
            return;
        }
        if self.program_status_word & Wrapping(0x7000) != Wrapping(0) {
            return;
        }
        if self.pending_interrupts == 0 {
            return;
        }
        if self.pending_interrupts == (1 << 7) {
            self.pending_interrupts &= !(1 << 7);
            self.interrupt(memory, 7);
        }
    }

    fn interrupt<M: Memory>(&mut self, memory: &mut M, level: u32) {
        memory
            .write_u32((self.isp - Wrapping(8)).0, self.pc.0)
            .unwrap();
        memory
            .write_u32((self.isp - Wrapping(4)).0, self.program_status_word.0)
            .unwrap();
        self.isp -= Wrapping(16);
        self.program_status_word = Wrapping(0);
        self.pc = Wrapping(memory
                               .read_u32((self.vector_base + Wrapping(16) +
                                          (Wrapping(4) * Wrapping(level)))
                                                 .0)
                               .unwrap());
    }

    pub fn step<M: Memory>(&mut self, memory: &mut M, symbols: Option<Symbols>) {
        // step the timers
        self.step_timers();

        // check if an interrupt should be serviced first
        //self.check_interrupts(memory);

        let (instruction, next) =
            match Instruction::decode(memory, self.pc.0, self.cpu_mode_escape) {
                Ok((i, next)) => (i, next),
                Err(Error::BusFault {
                        address: addr,
                        mode,
                        description,
                    }) => {
                    panic!("Bus fault from {:?} addr: 0x{:x} -- {:?}",
                           mode,
                           addr,
                           description);
                }
            };

        //let depth = self.depth;
        self.instruction_pc = self.pc;
        self.pc += Wrapping(next);
        self.cpu_mode_escape = false;
        //let next = next as usize;

/*
        if let Some(sym) = symbols.as_ref().and_then(|x| x.find(self.instruction_pc.0)) {
            println!("pc: 0x{:x} <{}>", self.instruction_pc.0, sym)
        } else {
            println!("pc: 0x{:x}", self.instruction_pc.0)
        };
*/

        let _len = self.pc.0 - self.instruction_pc.0;
/*
        print!("\t{}\n\t",
               Disasm(instruction, symbols, Some(self.instruction_pc.0)));
*/

        _ = self.explain(instruction, memory, symbols);

        /*for i in 0..depth {
            print!("-");
        }


        if let Some(sym) = symbols.as_ref().and_then(|x| x.find(self.instruction_pc.0)) {
            println!("|pc: 0x{:x} <{}>", self.instruction_pc.0, sym)
        } else {
            println!("|pc: 0x{:x}", self.instruction_pc.0)
        };*/



        /*match self.instruction_pc.0 {
            0x80c2c92...0x80c2d7e | 0x80c2d7e...0x80c2f70 => {
                println!("\tpsw: b{:08x}", self.program_status_word);
                println!("\tstack: 0x{:x}=>0x{:x}", self.sp, self.sp + Wrapping(4 * 4));
                for i in 0..32  {
                    let addr = self.sp + Wrapping(i * 4);
                    let val = memory.read_u32(addr.0);
                    if let Ok(val) = memory.read_u32(addr.0) {
                        print!("\t\tR{:<2}[0x{:08x}] = {2:11} (0x{3:08x})",
                               i * 4,
                               addr,
                               val as i32,
                               val);
                        if let Some(sym) = symbols.as_ref().and_then(|x| x.find(val)) {
                            println!(" <{}>", sym);
                        } else {
                            println!("");
                        }
                    } else {
                        println!("\t\tR{:<2}[0x{:08x}] = <invalid address>", i, addr);
                    }
                }
            }
            _ => ()
        }


        let sp = self.sp;
        for row in 0..40 {
            print!("{:08x}->{:08x}: ", self.sp.0 + (row * 8)*4, self.sp.0 + (row * 8 + 8)*4);
            for line in 0..8 {
                if let Ok(data) = memory.read_u32(self.sp.0 + (row * 8 + line)*4) {
                    print!("{:08x} ", data);    
                }
            }
            print!(" ");
            for line in 0..32 {
                if let Ok(data) = memory.read_u8(self.sp.0 + row * 32 + line) {
                    match data as char {
                        'a'...'z' | 'A'...'Z' | '0'...'9' | ' ' | '%' => {
                            print!("{}", data as char)
                        }
                        _ => print!(".")
                    }
                }
            }
            println!("");
        }*/


        if let Err(Error::BusFault {
                       address: addr,
                       mode,
                       description,
                   }) = self.execute(instruction, memory) {
            // roll back the pc
            self.pc = self.instruction_pc;


            /*let sp = self.sp;
            for row in 0..20 {
                print!("{:08x}->{:08x}: ", self.sp.0 + (row * 8)*4, self.sp.0 + (row * 16 + 8)*4);
                for line in 0..8 {
                    print!("{:08x} ", memory.read_u32(self.sp.0 + (row * 16 + line)*4).unwrap());
                } 
                println!("");
            }*/

            println!("Bus fault from {:?} addr: 0x{:x} -- {:?}", // was panic XXX
                   mode,
                   addr,
                   description);

        }


    }
}

trait SignExtend {
    fn extend(self) -> u32;
}

impl SignExtend for i8 {
    fn extend(self) -> u32 {
        (self as i32) as u32
    }
}

impl SignExtend for i16 {
    fn extend(self) -> u32 {
        (self as i32) as u32
    }
}

impl SignExtend for u16 {
    fn extend(self) -> u32 {
        // converting from a i16 to a i32 sign extens
        // converting from a u32 from a i32 leaves the bits intact
        ((self as i16) as i32) as u32
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Mode {
    AbsoluteByte(u32, bool),
    AbsoluteUnsignedByte(u32, bool),
    AbsoluteHalfword(u32, bool),
    AbsoluteUnsignedHalfword(u32, bool),
    AbsoluteWord(u32, bool),

    StackOffsetByte(u32),
    StackOffsetUnsignedByte(u32),
    StackOffsetHalfword(u32),
    StackOffsetUnsignedHalfword(u32),
    StackOffsetWord(u32),

    StackOffsetIndirectByte(u32),
    StackOffsetIndirectUnsignedByte(u32),
    StackOffsetIndirectHalfword(u32),
    StackOffsetIndirectUnsignedHalfword(u32),
    StackOffsetIndirectWord(u32),

    Immediate(u32),
    CpuRegister(u32),
    PCRelative(u32),
}

impl Mode {
    fn decode_parcel_monadict(mode: u16, p: [u16; 2]) -> Mode {
        let p = (p[0] as u32) << 16 | p[1] as u32;
        match mode & 0xF {
            0xC => Mode::AbsoluteWord(p, true),
            0xD => Mode::StackOffsetWord(p),
            0xE => Mode::PCRelative(p),
            0xF => Mode::Immediate(p),
            _ => panic!("handled monadic mode {:x}", mode),
        }
    }

    /// decode from a 3 parcels instruct
    ///
    fn decode_parcel_3(mode: u16, p: u16, cpu_mod: bool) -> Mode {
        match mode & 0xF {
            0x0 => Mode::AbsoluteByte(p as u32, true),
            0x1 => Mode::AbsoluteUnsignedByte(p as u32, true),
            0x2 => Mode::AbsoluteHalfword(p as u32, true),
            0x3 => Mode::AbsoluteUnsignedHalfword(p as u32, true),
            0x4 => Mode::StackOffsetByte(p.extend()),
            0x5 => Mode::StackOffsetUnsignedByte(p.extend()),
            0x6 => Mode::StackOffsetHalfword(p.extend()),
            0x7 => {
                if !cpu_mod {
                    Mode::StackOffsetUnsignedHalfword(p.extend())
                } else {
                    Mode::CpuRegister(p as u32)
                }
            }
            0x8 => Mode::StackOffsetIndirectByte(p.extend()),
            0x9 => Mode::StackOffsetIndirectUnsignedByte(p.extend()),
            0xA => Mode::StackOffsetIndirectHalfword(p.extend()),
            0xB => Mode::StackOffsetIndirectUnsignedHalfword(p.extend()),
            0xC => Mode::AbsoluteWord(p as u32, true),
            0xD => Mode::StackOffsetWord(p.extend()),
            0xE => Mode::StackOffsetIndirectWord(p.extend()),
            0xF => Mode::Immediate(p.extend()),
            _ => unreachable!(),
        }
    }

    /// decode from a 3 parcels instruct
    fn decode_parcel_5(mode: u16, p: [u16; 2], cpu_mod: bool) -> Mode {
        let p = (p[0] as u32) << 16 | p[1] as u32;
        match mode & 0xF {
            0x0 => Mode::AbsoluteByte(p, false),
            0x1 => Mode::AbsoluteUnsignedByte(p, false),
            0x2 => Mode::AbsoluteHalfword(p, false),
            0x3 => Mode::AbsoluteUnsignedHalfword(p, false),
            0x4 => Mode::StackOffsetByte(p),
            0x5 => Mode::StackOffsetUnsignedByte(p),
            0x6 => Mode::StackOffsetHalfword(p),
            0x7 => {
                if !cpu_mod {
                    Mode::StackOffsetUnsignedHalfword(p)
                } else {
                    Mode::CpuRegister(p)
                }
            }
            0x8 => Mode::StackOffsetIndirectByte(p),
            0x9 => Mode::StackOffsetIndirectUnsignedByte(p),
            0xA => Mode::StackOffsetIndirectHalfword(p),
            0xB => Mode::StackOffsetIndirectUnsignedHalfword(p),
            0xC => Mode::AbsoluteWord(p, false),
            0xD => Mode::StackOffsetWord(p),
            0xE => Mode::StackOffsetIndirectWord(p),
            0xF => Mode::Immediate(p),
            _ => unreachable!(),
        }
    }
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug)]
pub enum Dyadic {
    Add,
    And,
    Div,
    Mul,
    Or,
    Rem,
    Shl,
    Shr,
    Sub,
    Udiv,
    Urem,
    Ushl,
    Ushr,
    Xor,
}

impl Dyadic {
    pub fn eval(self, src: Wrapping<u32>, dst: Wrapping<u32>) -> Wrapping<u32> {
        match self {
            Dyadic::Add => dst + src,
            Dyadic::And => dst & src,
            Dyadic::Div => dst / src,
            Dyadic::Mul => dst * src,
            Dyadic::Or => dst | src,
            Dyadic::Rem => dst % src,
            // Dyadic::Shl => Wrapping(((dst.0 as i32) << (src.0 as usize)) as u32),
            Dyadic::Shl => Wrapping(((dst.0 as i32).checked_shl(src.0 as u32).unwrap_or(0)) as u32),

            Dyadic::Shr => Wrapping(((dst.0 as i32) >> (src.0 as usize)) as u32),
            Dyadic::Sub => dst - src,
            Dyadic::Udiv => dst / src,
            Dyadic::Urem => dst % src,
            Dyadic::Ushr => Wrapping(((dst.0 as i32) >> (src.0 as usize)) as u32),
            Dyadic::Ushl => Wrapping(((dst.0 as i32) >> (src.0 as usize)) as u32),
            Dyadic::Xor => dst ^ src,
        }
    }

    pub fn symbol(&self) -> &'static str {
        match *self {
            Dyadic::Add => "+",
            Dyadic::And => "&",
            Dyadic::Div => "/",
            Dyadic::Mul => "*",
            Dyadic::Or => "|",
            Dyadic::Rem => "%",
            Dyadic::Shl => "<<",
            Dyadic::Shr => ">>",
            Dyadic::Sub => "-",
            Dyadic::Udiv => "/",
            Dyadic::Urem => "%",
            Dyadic::Ushr => ">>",
            Dyadic::Ushl => "<<",
            Dyadic::Xor => "^",
        }
    }
}

impl fmt::Display for Dyadic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Dyadic::Add => write!(f, "add"),
            Dyadic::And => write!(f, "and"),
            Dyadic::Div => write!(f, "div"),
            Dyadic::Mul => write!(f, "mul"),
            Dyadic::Or => write!(f, "or"),
            Dyadic::Rem => write!(f, "rem"),
            Dyadic::Shl => write!(f, "shl"),
            Dyadic::Shr => write!(f, "shr"),
            Dyadic::Sub => write!(f, "sub"),
            Dyadic::Udiv => write!(f, "udiv"),
            Dyadic::Urem => write!(f, "urem"),
            Dyadic::Ushr => write!(f, "ushr"),
            Dyadic::Ushl => write!(f, "ushl"),
            Dyadic::Xor => write!(f, "xor"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum CmpMode {
    Eq,
    Gt,
    Hi,
}

impl CmpMode {
    fn eval(self, src: Wrapping<u32>, dst: Wrapping<u32>) -> bool {
        match self {
            CmpMode::Eq => src == dst,
            CmpMode::Gt => (src.0 as i32) > (dst.0 as i32),
            CmpMode::Hi => src > dst,
        }
    }

    pub fn symbol(&self) -> &'static str {
        match *self {
            CmpMode::Eq => "==",
            CmpMode::Gt | CmpMode::Hi => ">",
        }
    }
}

impl fmt::Display for CmpMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CmpMode::Eq => write!(f, "eq"),
            CmpMode::Gt => write!(f, "gt"),
            CmpMode::Hi => write!(f, "hi"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Instruction {
    CPU,
    Cret,
    Flushi,
    Flushp,
    Kret,
    Nop,

    Catch(Mode),
    Call(Mode),
    Enter(Mode),
    Jmp(Mode),
    JmpIf(Mode, bool, bool),
    Ldraa(Mode),
    Return(Mode),
    Kcall(Mode),

    /// dst = op(src, dst)
    Dyadic(Dyadic, Mode, Mode),
    /// acc = op(srd, dst)
    Dyadic3(Dyadic, Mode, Mode),
    /// interlocked version of Dyadic
    DyadicI(Dyadic, Mode, Mode),
    /// dst = src
    Mov(Mode, Mode),
    /// dst &= src
    Mova(Mode, Mode),
    ///
    Cmp(CmpMode, Mode, Mode),
    ///
    Dqm(Mode, Mode),

    UnknownParcels1(u16),
    UnknownParcels3([u16; 3]),
    UnknownParcels5([u16; 5]),
}


impl Instruction {
    /// decode an instruction with the offset of
    /// returns the number of bytes read to decode the instruction
    /// and the instruction
    pub fn decode<M: Memory>(memory: &mut M,
                             offset: u32,
                             cpu_mod: bool)
                             -> Result<(Instruction, u32), Error> {
        let p = memory.read_u16(offset)?;
        Ok(match p >> 14 & 0x3 {
               // if the 1st bit starts with a 0, the instruction is a single parcel
               0b00 | 0b01 => (Instruction::decode_parcel_1(p), 2),
               // for a instruction that takes 3 parcels
               0b10 => {
                   let p = [p,
                            memory.read_u16(offset + 2)?,
                            memory.read_u16(offset + 4)?];
                   (Instruction::decode_parcel_3(p, cpu_mod), 6)
               }
               // for all other (0b11) it is 5 parcels
               _ => {
                   let p = [p,
                            memory.read_u16(offset + 2)?,
                            memory.read_u16(offset + 4)?,
                            memory.read_u16(offset + 6)?,
                            memory.read_u16(offset + 8)?];
                   (Instruction::decode_parcel_5(p, cpu_mod), 10)
               }
           })
    }

    pub fn is_invalid(&self) -> bool {
        match *self {
            Instruction::UnknownParcels1(_) |
            Instruction::UnknownParcels3(_) |
            Instruction::UnknownParcels5(_) => true,
            _ => false,
        }
    }

    fn decode_parcel_1(p: u16) -> Instruction {
        //println!("decode: {:b}", p);
        fn imm5(imm: u16) -> Mode {
            Mode::Immediate(if imm & 0x10 != 0 {
                                0xffff_ffe0 | imm as u32
                            } else {
                                imm as u32
                            })
        }

        fn pcrel10(imm: u16) -> Mode {
            let imm = ((imm as u32) & 0x3FF) * 2;
            let imm = if imm & 0x400 == 0 {
                imm
            } else {
                0xffff_f800 | imm
            };
            //println!("imm:{}", imm);
            Mode::PCRelative(imm)
        }

        fn wai5(mut imm: u16) -> Mode {
            imm *= 4;
            Mode::Immediate(if imm & 0x40 != 0 {
                                0xffff_ff80 | imm as u32
                            } else {
                                imm as u32
                            })
        }

        fn stk5(imm: u16) -> Mode {
            Mode::StackOffsetWord(imm as u32 * 4)
        }

        fn istk5(imm: u16) -> Mode {
            Mode::StackOffsetIndirectWord(imm as u32 * 4)
        }

        let src = (p >> 5) & 0x1f;
        let dst = p & 0x1f;

        return match (p >> 10) & 0x1F {
                   0b00_000 => {
                       let src = ((p & 0x3FF) * 2) as u32;
                       let src = if src & 0x400 == 0 { src } else { 0xffff_f800 };
                       Instruction::Kcall(Mode::Immediate(src))
                   }
                   0b00_001 => Instruction::Call(pcrel10(p)),
                   // stack instructions
                   0b00_010 => {
                       let subcode = p & 0x3;
                       let src = (p & 0x3fc) as u32;
                       match subcode {
                           0b00 => {
                               // sign extend
                               let src = 0xFFFF_F000 | src * 4;
                               Instruction::Enter(Mode::StackOffsetWord(src))
                           }
                           0b01 => Instruction::Catch(Mode::StackOffsetWord(src * 4)),
                           0b10 => Instruction::Return(Mode::StackOffsetWord(src * 4)),
                           x => panic!("unhandled subcode 0b{:02b}", x),
                       }
                   }
                   0b00_011 => Instruction::Jmp(pcrel10(p)),
                   0b00_100 => Instruction::JmpIf(pcrel10(p), false, false),
                   0b00_101 => Instruction::JmpIf(pcrel10(p), false, true),
                   0b00_110 => Instruction::JmpIf(pcrel10(p), true, false),
                   0b00_111 => Instruction::JmpIf(pcrel10(p), true, true),
                   0b01_010 => Instruction::Mov(wai5(src), stk5(dst)),
                   // nildic instruction
                   0b01_011 => {
                       // last 10 bits
                       let subcode = p & 0x3FF;
                       match subcode {
                           0b00_0000_0000 => Instruction::CPU,
                           0b00_0000_0001 => Instruction::Kret,
                           0b00_0000_0010 => Instruction::Nop,
                           0b00_0000_0011 => Instruction::Flushi,
                           0b00_0000_0100 => Instruction::Flushp,
                           0b00_0000_0101 => Instruction::Cret,
                           _ => Instruction::UnknownParcels1(p),
                       }
                   }
                   0b01_101 => Instruction::Dyadic3(Dyadic::Add, wai5(src), stk5(dst)),
                   0b01_110 => Instruction::Dyadic3(Dyadic::And, imm5(src), stk5(dst)),
                   0b01_111 => Instruction::Dyadic(Dyadic::And, stk5(src), stk5(dst)),
                   0b10_000 => Instruction::Cmp(CmpMode::Eq, imm5(src), stk5(dst)),
                   0b10_001 => Instruction::Cmp(CmpMode::Gt, stk5(src), stk5(dst)),
                   0b10_010 => Instruction::Cmp(CmpMode::Gt, imm5(src), stk5(dst)),
                   0b10_011 => Instruction::Cmp(CmpMode::Eq, stk5(src), stk5(dst)),
                   0b10_100 => Instruction::Dyadic(Dyadic::Add, imm5(src), stk5(dst)),
                   0b10_101 => Instruction::Dyadic3(Dyadic::Add, imm5(src), stk5(dst)),
                   0b10_110 => Instruction::Dyadic(Dyadic::Add, stk5(src), stk5(dst)),
                   0b10_111 => Instruction::Dyadic3(Dyadic::Add, stk5(src), stk5(dst)),
                   0b11_000 => Instruction::Mov(stk5(src), stk5(dst)),
                   0b11_001 => Instruction::Mov(istk5(src), stk5(dst)),
                   0b11_010 => Instruction::Mov(stk5(src), istk5(dst)),
                   0b11_011 => Instruction::Mov(istk5(src), istk5(dst)),
                   0b11_100 => Instruction::Mov(imm5(src), stk5(dst)),
                   0b11_101 => Instruction::Mova(stk5(src), stk5(dst)),
                   0b11_110 => Instruction::Dyadic3(Dyadic::Shl, imm5(src), stk5(dst)),
                   0b11_111 => Instruction::Dyadic3(Dyadic::Shr, imm5(src), stk5(dst)),
                   _ => Instruction::UnknownParcels1(p),
               };
    }

    fn decode_dyadic(opcode: u16, src: Mode, dst: Mode) -> Option<Instruction> {
        Some(match opcode {
                 0b000_001 => Instruction::DyadicI(Dyadic::Or, src, dst),
                 0b000_010 => Instruction::DyadicI(Dyadic::And, src, dst),
                 0b000_100 => Instruction::Mova(src, dst),
                 0b000_101 => Instruction::Dyadic(Dyadic::Urem, src, dst),
                 0b000_110 => Instruction::Mov(src, dst),
                 0b000_111 => Instruction::Dqm(src, dst),
                 0b011_101 => Instruction::Cmp(CmpMode::Gt, src, dst),
                 0b011_110 => Instruction::Cmp(CmpMode::Hi, src, dst),
                 0b011_111 => Instruction::Cmp(CmpMode::Eq, src, dst),
                 0b100_000 => Instruction::Dyadic(Dyadic::Sub, src, dst),
                 0b100_001 => Instruction::Dyadic(Dyadic::Or, src, dst),
                 0b100_010 => Instruction::Dyadic(Dyadic::And, src, dst),
                 0b100_011 => Instruction::Dyadic(Dyadic::Add, src, dst),
                 0b100_101 => Instruction::Dyadic(Dyadic::Rem, src, dst),
                 0b100_110 => Instruction::Dyadic(Dyadic::Div, src, dst),
                 0b100_111 => Instruction::Dyadic(Dyadic::Mul, src, dst),
                 0b100_100 => Instruction::Dyadic(Dyadic::Xor, src, dst),
                 0b101_100 => Instruction::Dyadic(Dyadic::Shr, src, dst),
                 0b101_101 => Instruction::Dyadic(Dyadic::Ushr, src, dst),
                 0b101_110 => Instruction::Dyadic(Dyadic::Shl, src, dst),
                 0b101_111 => Instruction::Dyadic(Dyadic::Udiv, src, dst),
                 0b110_000 => Instruction::Dyadic3(Dyadic::Sub, src, dst),
                 0b110_001 => Instruction::Dyadic3(Dyadic::Or, src, dst),
                 0b110_010 => Instruction::Dyadic3(Dyadic::And, src, dst),
                 0b110_011 => Instruction::Dyadic3(Dyadic::Add, src, dst),
                 0b110_100 => Instruction::Dyadic3(Dyadic::Xor, src, dst),
                 0b110_101 => Instruction::Dyadic3(Dyadic::Rem, src, dst),
                 0b110_110 => Instruction::Dyadic3(Dyadic::Mul, src, dst),
                 0b110_111 => Instruction::Dyadic3(Dyadic::Div, src, dst),
                 0b111_100 => Instruction::Dyadic3(Dyadic::Shr, src, dst),
                 0b111_101 => Instruction::Dyadic3(Dyadic::Ushr, src, dst),
                 0b111_110 => Instruction::Dyadic3(Dyadic::Shl, src, dst),
                 _ => return None,
             })
    }

    fn decode_parcel_3(p: [u16; 3], cpu_mod: bool) -> Instruction {
        //println!("decode: {:b} {:b} {:b}", p[0], p[1], p[2]);
        //println!("{:04x} {:04x} {:04x}", p[0], p[1], p[2]);
        let opcode = (p[0] >> 8) & 0x3F;
        //let smode = Mode::decode_parcel_3((p[0] >> 4) & 0xF, p[1], cpu_mod);

        // check if the instruction is a monadic instruction
        if opcode == 0b000_000 {
            let smode = Mode::decode_parcel_monadict((p[0] >> 4) & 0xF, [p[1], p[2]]);
            return match p[0] & 0xf {
                       0b0_001 => Instruction::Call(smode),
                       0b0_011 => Instruction::Jmp(smode),
                       0b0_100 => Instruction::JmpIf(smode, false, false),
                       0b0_101 => Instruction::JmpIf(smode, false, true),
                       0b0_110 => Instruction::JmpIf(smode, true, false),
                       0b0_111 => Instruction::JmpIf(smode, true, false),
                       0b1_010 => Instruction::Ldraa(smode),
                       scode => panic!("unimplemented scode={:b}", scode),
                   };
        }

        let smode = Mode::decode_parcel_3((p[0] >> 4) & 0xF, p[1], cpu_mod);
        let dmode = Mode::decode_parcel_3(p[0] & 0xF, p[2], cpu_mod);
        Instruction::decode_dyadic(opcode, smode, dmode)
            .unwrap_or_else(|| Instruction::UnknownParcels3(p))
    }

    fn decode_parcel_5(p: [u16; 5], cpu_mod: bool) -> Instruction {
        //println!("decode: {:b} {:b} {:b} {:b} {:b}", p[0], p[1], p[2], p[3], p[4]);
        //println!("{:04x} {:04x} {:04x} {:04x} {:04x}", p[0], p[1], p[2], p[3], p[4]);
        let opcode = (p[0] >> 8) & 0x3F;
        let smode = Mode::decode_parcel_5((p[0] >> 4) & 0xF, [p[1], p[2]], cpu_mod);
        let dmode = Mode::decode_parcel_5(p[0] & 0xF, [p[3], p[4]], cpu_mod);
        Instruction::decode_dyadic(opcode, smode, dmode)
            .unwrap_or_else(|| Instruction::UnknownParcels5(p))
    }
}
