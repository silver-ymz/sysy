use anyhow::Result;
use koopa::ir::{
    dfg::DataFlowGraph, layout::BasicBlockNode, BasicBlock, BinaryOp, Function, Program, Value,
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

struct Context<'a, W: Write> {
    w: &'a mut W,
    program: &'a Program,
    dfg: Option<&'a DataFlowGraph>,
    register_alloc: RegisterAllocator,
    #[allow(dead_code)]
    nm: NameManager,
}

impl<'a, W: Write> Context<'a, W> {
    fn dfg(&self) -> &'a DataFlowGraph {
        self.dfg.unwrap()
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
            self.codegen_bb(bb, node)?;
        }

        Ok(())
    }

    fn codegen_bb(&mut self, _: BasicBlock, node: &BasicBlockNode) -> Result<()> {
        for &inst in node.insts().keys() {
            self.codegen_local_inst(inst)?;
        }

        Ok(())
    }

    fn codegen_local_inst(&mut self, inst: Value) -> Result<()> {
        use koopa::ir::ValueKind::*;
        let data = self.dfg().value(inst);
        match data.kind() {
            Integer(v) => {
                if v.value() == 0 {
                    self.register_alloc.set_x0(inst);
                    return Ok(());
                }
                let addr = self.register_alloc.get(inst);
                let ValueAddr::Register(reg) = addr else {
                    todo!()
                };
                writeln!(self.w, "  li {}, {}", reg.to_str(), v.value())?;
            }
            ZeroInit(_) => todo!(),
            Undef(_) => todo!(),
            Aggregate(_) => todo!(),
            FuncArgRef(_) => todo!(),
            BlockArgRef(_) => todo!(),
            Alloc(_) => todo!(),
            GlobalAlloc(_) => todo!(),
            Load(_) => todo!(),
            Store(_) => todo!(),
            GetPtr(_) => todo!(),
            GetElemPtr(_) => todo!(),
            Binary(_) => self.codegen_binary(inst)?,
            Branch(_) => todo!(),
            Jump(_) => todo!(),
            Call(_) => todo!(),
            Return(v) => {
                if let Some(v) = v.value() {
                    if !self.register_alloc.exist(v) {
                        self.codegen_local_inst(v)?;
                    }
                    let addr = self.register_alloc.get(v);
                    let ValueAddr::Register(reg) = addr else {
                        todo!()
                    };
                    if reg != Register::A0 {
                        writeln!(self.w, "  mv a0, {}", reg.to_str())?;
                    }
                }
                writeln!(self.w, "  ret")?
            }
        }

        Ok(())
    }

    fn codegen_binary(&mut self, inst: Value) -> Result<()> {
        let data = self.dfg().value(inst);
        let koopa::ir::ValueKind::Binary(v) = data.kind() else {
            unreachable!()
        };
        let addr = self.register_alloc.get(inst);
        let ValueAddr::Register(reg) = addr else {
            todo!()
        };
        if !self.register_alloc.exist(v.lhs()) {
            self.codegen_local_inst(v.lhs())?;
        }
        let lhs = self.register_alloc.get(v.lhs());
        let lhs = match lhs {
            ValueAddr::Register(reg) => reg,
            ValueAddr::Stack(_) => {
                todo!()
            }
        };
        if !self.register_alloc.exist(v.rhs()) {
            self.codegen_local_inst(v.rhs())?;
        }
        let rhs = self.register_alloc.get(v.rhs());
        let rhs = match rhs {
            ValueAddr::Register(reg) => reg,
            ValueAddr::Stack(_) => {
                todo!()
            }
        };

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

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Register {
    X0,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
}

impl Register {
    fn to_str(self) -> &'static str {
        match self {
            Register::X0 => "x0",
            Register::A0 => "a0",
            Register::A1 => "a1",
            Register::A2 => "a2",
            Register::A3 => "a3",
            Register::A4 => "a4",
            Register::A5 => "a5",
            Register::A6 => "a6",
            Register::A7 => "a7",
            Register::T0 => "t0",
            Register::T1 => "t1",
            Register::T2 => "t2",
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

#[derive(Debug, Clone, Copy)]
enum ValueAddr {
    Register(Register),
    #[allow(dead_code)]
    Stack(i32),
}

struct RegisterAllocator {
    used_cnt: u8,
    mem_offset: i32,
    map: HashMap<Value, ValueAddr>,
}

impl RegisterAllocator {
    fn new() -> Self {
        Self {
            used_cnt: 0,
            mem_offset: 0,
            map: HashMap::new(),
        }
    }

    fn exist(&self, value: Value) -> bool {
        self.map.contains_key(&value)
    }

    fn get(&mut self, value: Value) -> ValueAddr {
        if let Some(&res) = self.map.get(&value) {
            return res;
        }
        let res = self.alloc();
        self.map.insert(value, res);
        res
    }

    fn set_x0(&mut self, value: Value) {
        self.map.insert(value, ValueAddr::Register(Register::X0));
    }

    fn alloc(&mut self) -> ValueAddr {
        if let Some(reg) = self.alloc_reg() {
            ValueAddr::Register(reg)
        } else {
            self.mem_offset -= 4;
            ValueAddr::Stack(self.mem_offset)
        }
    }

    fn alloc_reg(&mut self) -> Option<Register> {
        let reg = match self.used_cnt {
            0 => Register::A0,
            1 => Register::A1,
            2 => Register::A2,
            3 => Register::A3,
            4 => Register::A4,
            5 => Register::A5,
            6 => Register::A6,
            7 => Register::A7,
            8 => Register::T0,
            9 => Register::T1,
            10 => Register::T2,
            11 => Register::T3,
            12 => Register::T4,
            13 => Register::T5,
            14 => Register::T6,
            _ => Register::A0,
        };
        self.used_cnt = (self.used_cnt + 1) % 15;
        Some(reg)
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
