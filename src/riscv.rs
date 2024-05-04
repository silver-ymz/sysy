use anyhow::Result;
use koopa::{
    back::{Generator, NameManager},
    ir::{
        dfg::DataFlowGraph,
        entities::{BasicBlockData, ValueData},
        layout::BasicBlockNode,
        FunctionData, Program,
    },
};
use std::io::Write;

pub type RiscvGenerator<W> = Generator<W, Visitor>;

#[derive(Default)]
pub struct Visitor;

impl<W: Write> koopa::back::Visitor<W> for Visitor {
    type Output = ();

    fn visit(
        &mut self,
        w: &mut W,
        nm: &mut NameManager,
        program: &Program,
    ) -> std::io::Result<Self::Output> {
        VisitorImpl {
            w,
            nm,
            program,
            dfg: None,
        }
        .visit()
        .map_err(std::io::Error::other)
    }
}

struct VisitorImpl<'a, W: Write> {
    w: &'a mut W,
    nm: &'a mut NameManager,
    program: &'a Program,
    dfg: Option<&'a DataFlowGraph>,
}

impl<'a, W: Write> VisitorImpl<'a, W> {
    fn visit(&mut self) -> Result<()> {
        write!(self.w, "  .text\n  .globl main\n")?;

        // TODO: deal with global insts

        for &func in self.program.func_layout() {
            let func = self.program.func(func);
            self.nm.enter_func_scope();
            self.dfg = Some(func.dfg());
            self.visit_func(func)?;
            self.nm.exit_func_scope();
        }

        Ok(())
    }

    fn visit_func(&mut self, func: &FunctionData) -> Result<()> {
        writeln!(self.w, "{}:", &self.nm.func_name(func)[1..])?;
        for (&bb, node) in func.layout().bbs() {
            let bb = self.dfg.unwrap().bb(bb);
            self.visit_bb(bb, node)?;
        }

        Ok(())
    }

    fn visit_bb(&mut self, bb: &BasicBlockData, node: &BasicBlockNode) -> Result<()> {
        writeln!(self.w, "{}:", &self.nm.bb_name(bb)[1..])?;
        for &inst in node.insts().keys() {
            let inst = self.dfg.unwrap().value(inst);
            self.visit_local_inst(inst)?;
        }

        Ok(())
    }

    fn visit_local_inst(&mut self, inst: &ValueData) -> Result<()> {
        use koopa::ir::ValueKind::*;
        match inst.kind() {
            Integer(v) => writeln!(self.w, "  li a0, {}", v.value())?,
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
            Binary(_) => todo!(),
            Branch(_) => todo!(),
            Jump(_) => todo!(),
            Call(_) => todo!(),
            Return(v) => {
                if let Some(v) = v.value() {
                    let v = self.dfg.unwrap().value(v);
                    self.visit_local_inst(v)?;
                }
                writeln!(self.w, "  ret")?
            }
        }

        Ok(())
    }
}
