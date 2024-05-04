use crate::ast::*;
use koopa::ir::{
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
    dfg::DataFlowGraph,
    BasicBlock, BinaryOp, FunctionData, Program, Type, Value,
};

pub fn codegen(unit: CompUnit) -> Program {
    let mut program = Program::new();
    let FuncDef {
        func_type,
        ident: func_name,
        block,
    } = unit.func_def;
    let stmt = block.stmt;

    let ret_ty = match func_type {
        FuncType::Int => Type::get_i32(),
    };
    let func = program.new_func(FunctionData::new(format!("@{}", func_name), vec![], ret_ty));
    let func = program.func_mut(func);

    Context { func, bb: None }.codegen_func(stmt);

    program
}

struct Context<'a> {
    func: &'a mut FunctionData,
    bb: Option<BasicBlock>,
}

impl<'a> Context<'a> {
    fn dfg_mut(&mut self) -> &mut DataFlowGraph {
        self.func.dfg_mut()
    }

    fn add_insts(&mut self, insts: &[Value]) {
        let bb = self.bb.unwrap();
        self.func
            .layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .extend(insts.into_iter().copied());
    }

    fn codegen_func(&mut self, stmt: Stmt) {
        let end = self.dfg_mut().new_bb().basic_block(None);
        self.func.layout_mut().bbs_mut().extend([end]);

        let Stmt { exp } = stmt;
        self.bb = Some(end);
        let ret_val = self.codegen_exp(exp);
        let ret = self.dfg_mut().new_value().ret(Some(ret_val));
        self.add_insts(&[ret]);
    }

    fn codegen_exp(&mut self, exp: Exp) -> Value {
        let Exp { exp } = exp;
        self.codegen_binary(exp)
    }

    fn codegen_binary(&mut self, exp: BinaryExp) -> Value {
        match exp {
            BinaryExp::Single(unary) => self.codegen_unary(unary),
            BinaryExp::Multi { op, lhs, rhs } => {
                let mut lhs_val = self.codegen_binary(*lhs);
                let mut rhs_val = self.codegen_binary(*rhs);
                if matches!(op, crate::ast::BinaryOp::LAnd | crate::ast::BinaryOp::LOr) {
                    let zero = self.dfg_mut().new_value().integer(0);
                    lhs_val = self
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::NotEq, zero, lhs_val);
                    rhs_val = self
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::NotEq, zero, rhs_val);
                    self.add_insts(&[lhs_val, rhs_val]);
                }

                let binary_op = match op {
                    crate::ast::BinaryOp::Add => BinaryOp::Add,
                    crate::ast::BinaryOp::Sub => BinaryOp::Sub,
                    crate::ast::BinaryOp::Mul => BinaryOp::Mul,
                    crate::ast::BinaryOp::Div => BinaryOp::Div,
                    crate::ast::BinaryOp::Mod => BinaryOp::Mod,
                    crate::ast::BinaryOp::Lt => BinaryOp::Lt,
                    crate::ast::BinaryOp::Le => BinaryOp::Le,
                    crate::ast::BinaryOp::Gt => BinaryOp::Gt,
                    crate::ast::BinaryOp::Ge => BinaryOp::Ge,
                    crate::ast::BinaryOp::Eq => BinaryOp::Eq,
                    crate::ast::BinaryOp::Ne => BinaryOp::NotEq,
                    crate::ast::BinaryOp::LAnd => BinaryOp::And,
                    crate::ast::BinaryOp::LOr => BinaryOp::Or,
                };
                let val = self
                    .dfg_mut()
                    .new_value()
                    .binary(binary_op, lhs_val, rhs_val);
                self.add_insts(&[val]);
                val
            }
        }
    }

    fn codegen_unary(&mut self, exp: UnaryExp) -> Value {
        match exp {
            UnaryExp::Primary(primary) => self.codegen_primary(primary),
            UnaryExp::Unary { op, exp } => {
                let val = self.codegen_unary(*exp);
                match op {
                    UnaryOp::Pos => val,
                    UnaryOp::Neg => {
                        let zero = self.dfg_mut().new_value().integer(0);
                        let neg = self.dfg_mut().new_value().binary(BinaryOp::Sub, zero, val);
                        self.add_insts(&[neg]);
                        neg
                    }
                    UnaryOp::Not => {
                        let zero = self.dfg_mut().new_value().integer(0);
                        let not = self.dfg_mut().new_value().binary(BinaryOp::Eq, zero, val);
                        self.add_insts(&[not]);
                        not
                    }
                }
            }
        }
    }

    fn codegen_primary(&mut self, primary: PrimaryExp) -> Value {
        match primary {
            PrimaryExp::Exp(exp) => self.codegen_exp(*exp),
            PrimaryExp::Num(num) => self.dfg_mut().new_value().integer(num),
        }
    }
}
