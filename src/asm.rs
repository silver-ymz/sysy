use anyhow::{anyhow, Result};
use koopa::ir::{
    dfg::DataFlowGraph, layout::BasicBlockNode, BasicBlock, BinaryOp, Function, Program, Value,
    ValueKind,
};
use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    io::Write,
};

pub fn codegen<W: Write>(w: &mut W, program: &Program) -> Result<()> {
    Context {
        w,
        program,
        dfg: None,
        register_alloc: RegisterAllocator::new(),
        nm: NameManager::new(),
    }
    .codegen()
}

struct Context<'a, W> {
    w: &'a mut W,
    program: &'a Program,
    dfg: Option<&'a DataFlowGraph>,
    register_alloc: RegisterAllocator,
    #[allow(dead_code)]
    nm: NameManager,
}

impl<'a, W: Write> Context<'a, W> {
    fn dfg(&self) -> Result<&'a DataFlowGraph> {
        self.dfg
            .ok_or(anyhow::anyhow!("DataFlowGraph is not set in Context"))
    }

    fn codegen(&mut self) -> Result<()> {
        write!(self.w, "  .text\n  .globl main\n")?;

        // TODO: deal with global insts

        for &func in self.program.func_layout() {
            self.codegen_func(func)?;
        }

        Ok(())
    }

    fn codegen_func(&mut self, func: Function) -> Result<()> {
        let func = self.program.func(func);
        self.dfg = Some(func.dfg());
        writeln!(self.w, "{}:", &func.name()[1..])?;

        for (&bb, node) in func.layout().bbs() {
            self.alloc_bb(bb, node)?;
        }
        self.register_alloc.finish();

        self.prologue()?;

        for (&bb, node) in func.layout().bbs() {
            self.codegen_bb(bb, node)?;
        }

        Ok(())
    }

    fn prologue(&mut self) -> Result<()> {
        writeln!(
            self.w,
            "  addi sp, sp, {}",
            self.register_alloc.stack_offset()
        )?;
        Ok(())
    }

    fn epilogue(&mut self) -> Result<()> {
        writeln!(
            self.w,
            "  addi sp, sp, {}",
            -self.register_alloc.stack_offset()
        )?;
        Ok(())
    }

    fn alloc_bb(&mut self, _: BasicBlock, node: &BasicBlockNode) -> Result<()> {
        for &inst in node.insts().keys() {
            self.alloc_local_inst(inst)?;
        }

        Ok(())
    }

    fn codegen_bb(&mut self, _: BasicBlock, node: &BasicBlockNode) -> Result<()> {
        for &inst in node.insts().keys() {
            self.codegen_local_inst(inst)?;
        }

        Ok(())
    }

    fn alloc_local_inst(&mut self, inst: Value) -> Result<ValueAddr> {
        use koopa::ir::ValueKind::*;
        let data = self.dfg()?.value(inst);
        match data.kind() {
            Integer(_) => Ok(ValueAddr::Register(Register::ZERO)),
            ZeroInit(_) => todo!(),
            Undef(_) => todo!(),
            Aggregate(_) => todo!(),
            FuncArgRef(_) => todo!(),
            BlockArgRef(_) => todo!(),
            Alloc(_) => Ok(self.register_alloc.set(inst)),
            GlobalAlloc(_) => todo!(),
            Load(_) => Ok(self.register_alloc.set(inst)),
            Store(_) => Ok(ValueAddr::Register(Register::ZERO)),
            GetPtr(_) => todo!(),
            GetElemPtr(_) => todo!(),
            Binary(_) => Ok(self.register_alloc.set(inst)),
            Branch(_) => todo!(),
            Jump(_) => todo!(),
            Call(_) => todo!(),
            Return(_) => Ok(ValueAddr::Register(Register::ZERO)),
        }
    }

    fn codegen_local_inst(&mut self, inst: Value) -> Result<()> {
        use koopa::ir::ValueKind::*;
        let data = self.dfg()?.value(inst);
        match data.kind() {
            Integer(_) => unreachable!(),
            ZeroInit(_) => todo!(),
            Undef(_) => todo!(),
            Aggregate(_) => todo!(),
            FuncArgRef(_) => todo!(),
            BlockArgRef(_) => todo!(),
            Alloc(_) => {}
            GlobalAlloc(_) => todo!(),
            Load(v) => {
                let src = self.register_alloc.get(v.src())?;
                let dst = self.register_alloc.get(inst)?;
                self.mov(src, dst)?;
            }
            Store(v) => {
                let value_data = self.dfg()?.value(v.value());
                if let ValueKind::Integer(num) = value_data.kind() {
                    match self.register_alloc.get(v.dest())? {
                        ValueAddr::Register(reg) => {
                            writeln!(self.w, "  li {}, {}", reg.to_str(), num.value())?;
                        }
                        ValueAddr::Stack(offset) => {
                            writeln!(self.w, "  li a0, {}", num.value())?;
                            writeln!(self.w, "  sw a0, {}(sp)", offset)?;
                        }
                    }
                    return Ok(());
                }

                let src = self.register_alloc.get(v.value())?;
                let dst = self.register_alloc.get(v.dest())?;
                self.mov(src, dst)?;
            }
            GetPtr(_) => todo!(),
            GetElemPtr(_) => todo!(),
            Binary(_) => self.codegen_binary(inst)?,
            Branch(_) => todo!(),
            Jump(_) => todo!(),
            Call(_) => todo!(),
            Return(v) => {
                if let Some(v) = v.value() {
                    let reg = self.load(v, Register::A0)?;
                    if reg != Register::A0 {
                        writeln!(self.w, "  mv a0, {}", reg.to_str())?;
                    }
                }
                self.epilogue()?;
                writeln!(self.w, "  ret")?
            }
        }

        Ok(())
    }

    fn codegen_binary(&mut self, inst: Value) -> Result<()> {
        let data = self.dfg()?.value(inst);
        let koopa::ir::ValueKind::Binary(v) = data.kind() else {
            unreachable!()
        };
        let addr = self.register_alloc.get(inst)?;
        let reg = match addr {
            ValueAddr::Register(reg) => reg,
            ValueAddr::Stack(_) => Register::A2,
        };
        let lhs = self.load(v.lhs(), Register::A0)?;
        let rhs = self.load(v.rhs(), Register::A1)?;

        match v.op() {
            BinaryOp::Add => {
                writeln!(
                    self.w,
                    "  add {}, {}, {}",
                    reg.to_str(),
                    lhs.to_str(),
                    rhs.to_str()
                )?;
            }
            BinaryOp::Sub => {
                writeln!(
                    self.w,
                    "  sub {}, {}, {}",
                    reg.to_str(),
                    lhs.to_str(),
                    rhs.to_str()
                )?;
            }
            BinaryOp::Mul => {
                writeln!(
                    self.w,
                    "  mul {}, {}, {}",
                    reg.to_str(),
                    lhs.to_str(),
                    rhs.to_str()
                )?;
            }
            BinaryOp::Div => {
                writeln!(
                    self.w,
                    "  div {}, {}, {}",
                    reg.to_str(),
                    lhs.to_str(),
                    rhs.to_str()
                )?;
            }
            BinaryOp::Mod => {
                writeln!(
                    self.w,
                    "  rem {}, {}, {}",
                    reg.to_str(),
                    lhs.to_str(),
                    rhs.to_str()
                )?;
            }
            BinaryOp::Lt => {
                writeln!(
                    self.w,
                    "  slt {}, {}, {}",
                    reg.to_str(),
                    lhs.to_str(),
                    rhs.to_str()
                )?;
            }
            BinaryOp::Le => {
                writeln!(
                    self.w,
                    "  sgt {}, {}, {}",
                    reg.to_str(),
                    lhs.to_str(),
                    rhs.to_str()
                )?;
                writeln!(self.w, "  seqz {}, {}", reg.to_str(), reg.to_str())?;
            }
            BinaryOp::Gt => {
                writeln!(
                    self.w,
                    "  sgt {}, {}, {}",
                    reg.to_str(),
                    lhs.to_str(),
                    rhs.to_str()
                )?;
            }
            BinaryOp::Ge => {
                writeln!(
                    self.w,
                    "  slt {}, {}, {}",
                    reg.to_str(),
                    lhs.to_str(),
                    rhs.to_str()
                )?;
                writeln!(self.w, "  seqz {}, {}", reg.to_str(), reg.to_str())?;
            }
            BinaryOp::Eq => {
                writeln!(
                    self.w,
                    "  xor {}, {}, {}",
                    reg.to_str(),
                    lhs.to_str(),
                    rhs.to_str()
                )?;
                writeln!(self.w, "  seqz {}, {}", reg.to_str(), reg.to_str())?;
            }
            BinaryOp::NotEq => {
                writeln!(
                    self.w,
                    "  xor {}, {}, {}",
                    reg.to_str(),
                    lhs.to_str(),
                    rhs.to_str()
                )?;
                writeln!(self.w, "  snez {}, {}", reg.to_str(), reg.to_str())?;
            }
            BinaryOp::And => {
                writeln!(
                    self.w,
                    "  and {}, {}, {}",
                    reg.to_str(),
                    lhs.to_str(),
                    rhs.to_str()
                )?;
            }
            BinaryOp::Or => {
                writeln!(
                    self.w,
                    "  or {}, {}, {}",
                    reg.to_str(),
                    lhs.to_str(),
                    rhs.to_str()
                )?;
            }
            _ => todo!(),
        }

        if let ValueAddr::Stack(offset) = addr {
            writeln!(self.w, "  sw {}, {}(sp)", reg.to_str(), offset)?;
        }

        Ok(())
    }

    fn load(&mut self, value: Value, fallback: Register) -> Result<Register> {
        let data = self.dfg()?.value(value);
        if let ValueKind::Integer(v) = data.kind() {
            if v.value() == 0 {
                return Ok(Register::ZERO);
            }
            writeln!(self.w, "  li {}, {}", fallback.to_str(), v.value())?;
            return Ok(fallback);
        }

        match self.register_alloc.get(value)? {
            ValueAddr::Register(reg) => Ok(reg),
            ValueAddr::Stack(offset) => {
                writeln!(self.w, "  lw {}, {}(sp)", fallback.to_str(), offset)?;
                Ok(fallback)
            }
        }
    }

    fn mov(&mut self, src: ValueAddr, dst: ValueAddr) -> Result<()> {
        match (src, dst) {
            (ValueAddr::Register(src), ValueAddr::Register(dst)) => {
                writeln!(self.w, "  mv {}, {}", dst.to_str(), src.to_str())?;
            }
            (ValueAddr::Stack(src), ValueAddr::Register(dst)) => {
                writeln!(self.w, "  lw {}, {}(sp)", dst.to_str(), src)?;
            }
            (ValueAddr::Register(src), ValueAddr::Stack(dst)) => {
                writeln!(self.w, "  sw {}, {}(sp)", src.to_str(), dst)?;
            }
            (ValueAddr::Stack(src), ValueAddr::Stack(dst)) => {
                writeln!(self.w, "  lw a0, {}(sp)", src)?;
                writeln!(self.w, "  sw a0, {}(sp)", dst)?;
            }
        }

        Ok(())
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Register {
    ZERO = 0,
    RA,
    SP,
    GP,
    TP,
    T0,
    T1,
    T2,
    FP,
    S1,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
    T3,
    T4,
    T5,
    T6,
}

impl Register {
    fn to_str(self) -> &'static str {
        match self {
            Register::ZERO => "x0",
            Register::RA => "ra",
            Register::SP => "sp",
            Register::GP => "gp",
            Register::TP => "tp",
            Register::T0 => "t0",
            Register::T1 => "t1",
            Register::T2 => "t2",
            Register::FP => "s0",
            Register::S1 => "s1",
            Register::A0 => "a0",
            Register::A1 => "a1",
            Register::A2 => "a2",
            Register::A3 => "a3",
            Register::A4 => "a4",
            Register::A5 => "a5",
            Register::A6 => "a6",
            Register::A7 => "a7",
            Register::S2 => "s2",
            Register::S3 => "s3",
            Register::S4 => "s4",
            Register::S5 => "s5",
            Register::S6 => "s6",
            Register::S7 => "s7",
            Register::S8 => "s8",
            Register::S9 => "s9",
            Register::S10 => "s10",
            Register::S11 => "s11",
            Register::T3 => "t3",
            Register::T4 => "t4",
            Register::T5 => "t5",
            Register::T6 => "t6",
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

const TEMP_REGS: &[Register] = &[
    Register::T0,
    Register::T1,
    Register::T2,
    Register::T3,
    Register::T4,
    Register::T5,
    Register::T6,
    Register::A3,
    Register::A4,
    Register::A5,
    Register::A6,
    Register::A7,
];

#[derive(Debug, Clone, Copy)]
enum ValueAddr {
    Register(Register),
    Stack(i32),
}

struct RegisterAllocator {
    reg_bitmap: u32,
    stack_offset: i32,
    avail_stack: Vec<i32>,
    map: HashMap<Value, ValueAddr>,
}

impl RegisterAllocator {
    fn new() -> Self {
        Self {
            reg_bitmap: 0,
            stack_offset: 0,
            avail_stack: Vec::new(),
            map: HashMap::new(),
        }
    }

    fn get(&self, value: Value) -> Result<ValueAddr> {
        self.map
            .get(&value)
            .copied()
            .ok_or(anyhow!("Value is not allocated: {:?}", value))
    }

    fn set(&mut self, value: Value) -> ValueAddr {
        if let Some(&res) = self.map.get(&value) {
            return res;
        }
        let res = self.alloc();
        self.map.insert(value, res);
        res
    }

    fn alloc(&mut self) -> ValueAddr {
        if let Some(reg) = self.alloc_reg() {
            ValueAddr::Register(reg)
        } else if let Some(offset) = self.avail_stack.pop() {
            ValueAddr::Stack(offset)
        } else {
            self.stack_offset -= 4;
            ValueAddr::Stack(self.stack_offset)
        }
    }

    #[allow(dead_code)]
    fn free(&mut self, value: ValueAddr) {
        match value {
            ValueAddr::Register(reg) => self.reg_bitmap &= !(1 << reg as u32),
            ValueAddr::Stack(offset) => {
                self.avail_stack.push(offset);
            }
        }
    }

    fn alloc_reg(&mut self) -> Option<Register> {
        for &reg in TEMP_REGS {
            if self.reg_bitmap & (1 << reg as u32) == 0 {
                self.reg_bitmap |= 1 << reg as u32;
                return Some(reg);
            }
        }
        None
    }

    fn finish(&mut self) {
        self.stack_offset = (self.stack_offset - 15) & !0xf;
        for offset in self.map.values_mut() {
            if let ValueAddr::Stack(offset) = offset {
                *offset -= self.stack_offset;
            }
        }
    }

    fn stack_offset(&self) -> i32 {
        self.stack_offset
    }
}

struct NameManager {
    #[allow(dead_code)]
    global_count: usize,
}

impl NameManager {
    fn new() -> Self {
        Self { global_count: 0 }
    }
}
