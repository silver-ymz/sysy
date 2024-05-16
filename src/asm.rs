use anyhow::{anyhow, Result};
use koopa::ir::{
    dfg::DataFlowGraph, layout::BasicBlockNode, BasicBlock, BinaryOp, Function, Program, TypeKind,
    Value, ValueKind,
};
use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    io::Write,
    rc::Rc,
};

pub fn codegen<W: Write>(w: &mut W, program: &Program) -> Result<()> {
    Context {
        w,
        program,
        dfg: None,
        register_alloc: Vec::new(),
        nm: NameManager::new(),
        global_vars: None,
    }
    .codegen()
}

struct Context<'a, W> {
    w: &'a mut W,
    program: &'a Program,
    dfg: Option<&'a DataFlowGraph>,
    register_alloc: Vec<RegisterAllocator>,
    nm: NameManager,
    global_vars: Option<Rc<HashMap<Value, String>>>,
}

impl<'a, W: Write> Context<'a, W> {
    fn dfg(&self) -> Result<&'a DataFlowGraph> {
        self.dfg
            .ok_or(anyhow::anyhow!("DataFlowGraph is not set in Context"))
    }

    fn register_alloc(&mut self) -> Result<&mut RegisterAllocator> {
        self.register_alloc
            .last_mut()
            .ok_or(anyhow!("RegisterAllocator is not set"))
    }

    fn codegen(&mut self) -> Result<()> {
        self.codegen_global_vars()?;

        write!(self.w, "  .text\n  .globl main\n")?;
        for &func in self.program.func_layout() {
            self.codegen_func(func)?;
        }

        Ok(())
    }

    fn codegen_global_vars(&mut self) -> Result<()> {
        let mut global_vars = HashMap::new();
        if !self.program.inst_layout().is_empty() {
            writeln!(self.w, "  .data")?;
            let mut nm_count = 0;
            for &inst in self.program.inst_layout() {
                let data = self.program.borrow_value(inst);
                let name = match data.name().as_ref() {
                    Some(name) => format!(".V{}", &name[1..]),
                    None => {
                        let name = format!(".V{}", nm_count);
                        nm_count += 1;
                        name
                    }
                };

                writeln!(self.w, "  .globl {}", name)?;
                writeln!(self.w, "{}:", name)?;
                global_vars.insert(inst, name);
                let ValueKind::GlobalAlloc(data) = data.kind() else {
                    return Err(anyhow!("global variable is not GlobalAlloc: {:?}", data));
                };
                let data = self.program.borrow_value(data.init());
                match data.kind() {
                    ValueKind::Integer(num) => {
                        writeln!(self.w, "  .word {}", num.value())?;
                    }
                    ValueKind::ZeroInit(_) => {
                        let size = data.ty().size();
                        writeln!(self.w, "  .zero {}", size)?;
                    }
                    other => todo!("global_vars: {:?}", other),
                }
            }
            writeln!(self.w)?;
        }
        self.global_vars = Some(Rc::new(global_vars));

        Ok(())
    }

    fn codegen_func(&mut self, func: Function) -> Result<()> {
        let func = self.program.func(func);
        if func.dfg().bbs().is_empty() {
            return Ok(());
        }

        self.dfg = Some(func.dfg());
        writeln!(self.w, "{}:", &func.name()[1..])?;

        let global_vars = self.global_vars.as_ref().unwrap().clone();
        self.register_alloc
            .push(RegisterAllocator::new(global_vars));
        self.register_alloc()?.set_params(func.params());
        for (&bb, node) in func.layout().bbs() {
            self.alloc_bb(bb, node)?;
        }
        self.register_alloc()?.finish();

        self.prologue()?;

        for (&bb, node) in func.layout().bbs() {
            self.codegen_bb(bb, node)?;
        }

        self.register_alloc.pop();

        Ok(())
    }

    fn prologue(&mut self) -> Result<()> {
        if self.register_alloc()?.max_call_alloc.is_some() {
            writeln!(self.w, "  sw ra, -4(sp)")?;
        }
        let stack_offset = self.register_alloc()?.stack_offset();
        if stack_offset < 0 {
            writeln!(self.w, "  addi sp, sp, {}", stack_offset)?;
        }
        Ok(())
    }

    fn epilogue(&mut self) -> Result<()> {
        let stack_offset = self.register_alloc()?.stack_offset();
        if stack_offset < 0 {
            writeln!(self.w, "  addi sp, sp, {}", -stack_offset)?;
        }
        if self.register_alloc()?.max_call_alloc.is_some() {
            writeln!(self.w, "  lw ra, -4(sp)")?;
        }
        Ok(())
    }

    fn alloc_bb(&mut self, bb: BasicBlock, node: &BasicBlockNode) -> Result<()> {
        self.nm.alloc(bb);
        for &inst in node.insts().keys() {
            self.alloc_local_inst(inst)?;
        }

        Ok(())
    }

    fn codegen_bb(&mut self, bb: BasicBlock, node: &BasicBlockNode) -> Result<()> {
        writeln!(self.w, "{}:", self.nm.get(bb)?)?;
        for &inst in node.insts().keys() {
            self.codegen_local_inst(inst)?;
        }

        Ok(())
    }

    fn alloc_local_inst(&mut self, inst: Value) -> Result<ValueAddr> {
        use koopa::ir::ValueKind::*;
        let data = self.dfg()?.value(inst);
        match data.kind() {
            Integer(_) => Ok(ValueAddr::Register(Register::X0)),
            ZeroInit(_) => todo!(),
            Undef(_) => todo!(),
            Aggregate(_) => todo!(),
            FuncArgRef(_) => todo!(),
            BlockArgRef(_) => todo!(),
            Alloc(_) => Ok(self.register_alloc()?.set(inst)),
            GlobalAlloc(_) => todo!(),
            Load(_) => Ok(self.register_alloc()?.set(inst)),
            Store(_) => Ok(ValueAddr::Register(Register::X0)),
            GetPtr(_) => todo!(),
            GetElemPtr(_) => todo!(),
            Binary(_) => Ok(self.register_alloc()?.set(inst)),
            Branch(_) => Ok(ValueAddr::Register(Register::X0)),
            Jump(_) => Ok(ValueAddr::Register(Register::X0)),
            Call(v) => {
                self.register_alloc()?.alloc_call_args(v.args());
                let ty = self.program.func(v.callee()).ty();
                let ret_ty = match ty.kind() {
                    TypeKind::Function(_, ret) => ret,
                    _ => unreachable!(),
                };
                if ret_ty.is_i32() {
                    Ok(self.register_alloc()?.set(inst))
                } else {
                    Ok(ValueAddr::Register(Register::X0))
                }
            }
            Return(_) => Ok(ValueAddr::Register(Register::X0)),
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
                let src = self.register_alloc()?.get(v.src())?;
                let dst = self.register_alloc()?.get(inst)?;
                self.mov(src, dst)?;
            }
            Store(v) => {
                let value_data = self.dfg()?.value(v.value());
                if let ValueKind::Integer(num) = value_data.kind() {
                    let dst = self.register_alloc()?.get(v.dest())?;
                    self.mov_const(num.value(), dst)?;
                    return Ok(());
                }

                let src = self.register_alloc()?.get(v.value())?;
                let dst = self.register_alloc()?.get(v.dest())?;
                self.mov(src, dst)?;
            }
            GetPtr(_) => todo!(),
            GetElemPtr(_) => todo!(),
            Binary(_) => self.codegen_binary(inst)?,
            Branch(v) => {
                if !v.true_args().is_empty() || !v.false_args().is_empty() {
                    todo!()
                }

                let cond_val = v.cond();
                let reg = self.load(cond_val, Register::A0)?;
                let then_label = self.nm.get(v.true_bb())?;
                let else_label = self.nm.get(v.false_bb())?;
                writeln!(self.w, "  bnez {}, {}", reg.to_str(), then_label)?;
                writeln!(self.w, "  j {}", else_label)?;
            }
            Jump(v) => {
                let label = self.nm.get(v.target())?;
                writeln!(self.w, "  j {}", label)?;
            }
            Call(v) => {
                for (i, &arg) in v.args().iter().enumerate().rev() {
                    let addr = match i {
                        0 => ValueAddr::Register(Register::A0),
                        1 => ValueAddr::Register(Register::A1),
                        2 => ValueAddr::Register(Register::A2),
                        3 => ValueAddr::Register(Register::A3),
                        4 => ValueAddr::Register(Register::A4),
                        5 => ValueAddr::Register(Register::A5),
                        6 => ValueAddr::Register(Register::A6),
                        7 => ValueAddr::Register(Register::A7),
                        n => ValueAddr::Stack(4 * (n as i32 - 8)),
                    };
                    let arg_data = self.dfg()?.value(arg);
                    if let ValueKind::Integer(num) = arg_data.kind() {
                        self.mov_const(num.value(), addr)?;
                    } else {
                        let src = self.register_alloc()?.get(arg)?;
                        self.mov(src, addr)?;
                    }
                }
                let func_data = self.program.func(v.callee());
                writeln!(self.w, "  call {}", &func_data.name()[1..])?;
                let ret_ty = match func_data.ty().kind() {
                    TypeKind::Function(_, ret) => ret,
                    _ => unreachable!(),
                };
                if ret_ty.is_i32() {
                    let dst = self.register_alloc()?.get(inst)?;
                    self.mov(ValueAddr::Register(Register::A0), dst)?;
                }
            }
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
        let addr = self.register_alloc()?.get(inst)?;
        let reg = match addr {
            ValueAddr::Register(reg) => reg,
            _ => Register::A2,
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

        match addr {
            ValueAddr::Register(_) => {}
            ValueAddr::Stack(offset) => {
                writeln!(self.w, "  sw {}, {}(sp)", reg.to_str(), offset)?;
            }
            ValueAddr::Global(name) => {
                writeln!(self.w, "  la a0, {}", name)?;
                writeln!(self.w, "  sw {}, (a0)", reg.to_str())?;
            }
        }

        Ok(())
    }

    fn load(&mut self, value: Value, fallback: Register) -> Result<Register> {
        let data = self.dfg()?.value(value);
        if let ValueKind::Integer(v) = data.kind() {
            if v.value() == 0 {
                return Ok(Register::X0);
            }
            writeln!(self.w, "  li {}, {}", fallback.to_str(), v.value())?;
            return Ok(fallback);
        }

        match self.register_alloc()?.get(value)? {
            ValueAddr::Register(reg) => Ok(reg),
            ValueAddr::Stack(offset) => {
                writeln!(self.w, "  lw {}, {}(sp)", fallback.to_str(), offset)?;
                Ok(fallback)
            }
            ValueAddr::Global(name) => {
                writeln!(self.w, "  lw {}, {}", fallback.to_str(), name)?;
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
            (ValueAddr::Global(name), ValueAddr::Register(dst)) => {
                writeln!(self.w, "  lw {}, {}", dst.to_str(), name)?;
            }
            (ValueAddr::Register(src), ValueAddr::Stack(dst)) => {
                writeln!(self.w, "  sw {}, {}(sp)", src.to_str(), dst)?;
            }
            (ValueAddr::Stack(src), ValueAddr::Stack(dst)) => {
                writeln!(self.w, "  lw a0, {}(sp)", src)?;
                writeln!(self.w, "  sw a0, {}(sp)", dst)?;
            }
            (ValueAddr::Global(name), ValueAddr::Stack(dst)) => {
                writeln!(self.w, "  lw a0, {}", name)?;
                writeln!(self.w, "  sw a0, {}(sp)", dst)?;
            }
            (ValueAddr::Register(src), ValueAddr::Global(name)) => {
                writeln!(self.w, "  la a0, {}", name)?;
                writeln!(self.w, "  sw {}, (a0)", src.to_str())?;
            }
            (ValueAddr::Stack(src), ValueAddr::Global(name)) => {
                writeln!(self.w, "  lw a0, {}(sp)", src)?;
                writeln!(self.w, "  la a1, {}", name)?;
                writeln!(self.w, "  sw a0, (a1)")?;
            }
            (ValueAddr::Global(src), ValueAddr::Global(dst)) => {
                writeln!(self.w, "  lw a0, {}", src)?;
                writeln!(self.w, "  la a1, {}", dst)?;
                writeln!(self.w, "  sw a0, (a1)")?;
            }
        }

        Ok(())
    }

    fn mov_const(&mut self, num: i32, dst: ValueAddr) -> Result<()> {
        match dst {
            ValueAddr::Register(reg) => {
                writeln!(self.w, "  li {}, {}", reg.to_str(), num)?;
            }
            ValueAddr::Stack(offset) => {
                writeln!(self.w, "  li a0, {}", num)?;
                writeln!(self.w, "  sw a0, {}(sp)", offset)?;
            }
            ValueAddr::Global(name) => {
                writeln!(self.w, "  li a0, {}", num)?;
                writeln!(self.w, "  la a1, {}", name)?;
                writeln!(self.w, "  sw a0, (a1)")?;
            }
        }

        Ok(())
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Register {
    X0 = 0,
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
            Register::X0 => "x0",
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

#[derive(Debug, Clone)]
enum ValueAddr {
    Register(Register),
    Stack(i32),
    Global(String),
}

struct RegisterAllocator {
    stack_offset: i32,
    max_call_alloc: Option<u32>,
    map: HashMap<Value, ValueAddr>,
    global_vars: Rc<HashMap<Value, String>>,
}

impl RegisterAllocator {
    fn new(global_vars: Rc<HashMap<Value, String>>) -> Self {
        Self {
            stack_offset: 0,
            max_call_alloc: None,
            map: HashMap::new(),
            global_vars,
        }
    }

    fn get(&self, value: Value) -> Result<ValueAddr> {
        if let Some(res) = self.map.get(&value) {
            return Ok(res.clone());
        }
        if let Some(name) = self.global_vars.get(&value) {
            return Ok(ValueAddr::Global(name.clone()));
        }
        Err(anyhow!("Value is not allocated: {:?}", value))
    }

    fn set(&mut self, value: Value) -> ValueAddr {
        if let Some(res) = self.map.get(&value) {
            return res.clone();
        }
        let res = self.alloc();
        self.map.insert(value, res.clone());
        res
    }

    fn set_params(&mut self, values: &[Value]) {
        for (i, &value) in values.iter().enumerate() {
            let addr = match i {
                0 => ValueAddr::Register(Register::A0),
                1 => ValueAddr::Register(Register::A1),
                2 => ValueAddr::Register(Register::A2),
                3 => ValueAddr::Register(Register::A3),
                4 => ValueAddr::Register(Register::A4),
                5 => ValueAddr::Register(Register::A5),
                6 => ValueAddr::Register(Register::A6),
                7 => ValueAddr::Register(Register::A7),
                n => ValueAddr::Stack(4 * (n as i32 - 8)),
            };
            self.map.insert(value, addr);
        }
    }

    fn alloc_call_args(&mut self, params: &[Value]) {
        let len = params.len() as u32;
        let num = std::cmp::max(len, 8) - 8;
        match self.max_call_alloc {
            Some(n) => self.max_call_alloc = Some(std::cmp::max(n, num)),
            None => self.max_call_alloc = Some(num),
        }
    }

    fn alloc(&mut self) -> ValueAddr {
        self.stack_offset -= 4;
        ValueAddr::Stack(self.stack_offset)
    }

    fn finish(&mut self) {
        let alloc_ra = self.max_call_alloc.is_some() as i32;
        if let Some(n) = self.max_call_alloc {
            self.stack_offset -= 4 * (n as i32 + 1);
        }
        self.stack_offset &= !0xf;
        for offset in self.map.values_mut() {
            if let ValueAddr::Stack(offset) = offset {
                *offset -= self.stack_offset + alloc_ra * 4;
            }
        }
    }

    fn stack_offset(&self) -> i32 {
        self.stack_offset
    }
}

struct NameManager {
    count: usize,
    map: HashMap<BasicBlock, usize>,
}

impl NameManager {
    fn new() -> Self {
        Self {
            count: 0,
            map: HashMap::new(),
        }
    }

    fn alloc(&mut self, bb: BasicBlock) {
        self.map.insert(bb, self.count);
        self.count += 1;
    }

    fn get(&self, bb: BasicBlock) -> Result<String> {
        match self.map.get(&bb) {
            Some(&count) => Ok(format!(".L{}", count)),
            None => Err(anyhow!("BasicBlock is not alloc: {:?}", bb)),
        }
    }
}
