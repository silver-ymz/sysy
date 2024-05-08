use crate::ast::*;
use anyhow::{anyhow, Result};
use koopa::ir::{
    builder::{BasicBlockBuilder, LocalBuilder, LocalInstBuilder, ValueBuilder},
    dfg::DataFlowGraph,
    layout::Layout,
    BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value, ValueKind,
};
use std::collections::HashMap;

pub fn codegen(unit: CompUnit) -> Result<Program> {
    let mut program = Program::new();
    Context {
        program: &mut program,
        func: None,
        bb: None,
        symbol_table: ScopeSymbolTable::new(),
    }
    .codegen(unit)?;
    Ok(program)
}

struct Context<'a> {
    program: &'a mut Program,
    func: Option<Function>,
    bb: Option<BasicBlock>,
    symbol_table: ScopeSymbolTable,
}

impl<'a> Context<'a> {
    fn bb(&self) -> Result<BasicBlock> {
        self.bb.ok_or(anyhow!("Basic Block is not set in Context"))
    }

    fn func_mut(&mut self) -> Result<&mut FunctionData> {
        let func = self.func.ok_or(anyhow!("Function is not set in Context"))?;
        let res = self.program.func_mut(func);
        Ok(res)
    }

    fn dfg_mut(&mut self) -> Result<&mut DataFlowGraph> {
        Ok(self.func_mut()?.dfg_mut())
    }

    fn layout_mut(&mut self) -> Result<&mut Layout> {
        Ok(self.func_mut()?.layout_mut())
    }

    fn add_insts(&mut self, insts: &[Value]) -> Result<()> {
        let bb = self.bb()?;
        self.layout_mut()?
            .bb_mut(bb)
            .insts_mut()
            .extend(insts.into_iter().copied());
        Ok(())
    }

    fn add_bbs(&mut self, bbs: &[BasicBlock]) -> Result<()> {
        self.layout_mut()?
            .bbs_mut()
            .extend(bbs.into_iter().copied());
        Ok(())
    }

    fn new_bb(&mut self) -> Result<BasicBlock> {
        Ok(self.dfg_mut()?.new_bb().basic_block(None))
    }

    fn new_value(&mut self) -> Result<LocalBuilder> {
        Ok(self.dfg_mut()?.new_value())
    }

    fn take_const(&mut self, value: Value) -> Result<Option<i32>> {
        let res = match self.dfg_mut()?.value(value).kind() {
            ValueKind::Integer(num) => Some(num.value()),
            _ => None,
        };
        Ok(res)
    }

    fn codegen(&mut self, unit: CompUnit) -> Result<()> {
        self.codegen_func(unit.func_def)
    }

    fn codegen_func(&mut self, func: FuncDef) -> Result<()> {
        let FuncDef {
            func_type,
            ident: func_name,
            block,
        } = func;

        let ret_ty = match func_type {
            FuncType::Int => Type::get_i32(),
        };
        let func =
            self.program
                .new_func(FunctionData::new(format!("@{}", func_name), vec![], ret_ty));
        self.func = Some(func);

        let end = self.new_bb()?;
        self.add_bbs(&[end])?;
        self.bb = Some(end);

        if !self.codegen_block(block)? {
            return Err(anyhow!("no return statement in function"));
        }

        Ok(())
    }

    fn codegen_block(&mut self, block: Block) -> Result<bool> {
        let mut res = false;
        self.symbol_table.enter_scope();
        for item in block.items {
            match item {
                BlockItem::Decl(decl) => self.codegen_decl(decl)?,
                BlockItem::Stmt(stmt) => {
                    if self.codegen_stmt(stmt)? {
                        res = true;
                        break;
                    }
                }
            }
        }
        self.symbol_table.exit_scope()?;

        Ok(res)
    }

    fn codegen_decl(&mut self, decl: Decl) -> Result<()> {
        match decl {
            Decl::Const(const_decl) => self.codegen_const_decl(const_decl),
            Decl::Var(var_decl) => self.codegen_var_decl(var_decl),
        }
    }

    fn codegen_const_decl(&mut self, const_decl: ConstDecl) -> Result<()> {
        let ConstDecl { btype, defs } = const_decl;
        assert!(matches!(btype, BType::Int));

        for def in defs {
            let ConstDef { ident, val } = def;
            let num = self.codegen_exp(val)?;
            let num = self
                .take_const(num)?
                .ok_or(anyhow!("non-constant expression: {}", ident))?;
            if !self.symbol_table.insert(ident.clone(), Val::Const(num))? {
                return Err(anyhow!("redefinition of constant: {}", ident));
            }
        }

        Ok(())
    }

    fn codegen_var_decl(&mut self, var_decl: VarDecl) -> Result<()> {
        let VarDecl { btype, defs } = var_decl;
        let ty = match btype {
            BType::Int => Type::get_i32(),
        };

        for def in defs {
            let VarDef { ident, val } = def;
            let var = self.new_value()?.alloc(ty.clone());
            self.add_insts(&[var])?;
            if let Some(exp) = val {
                let val = self.codegen_exp(exp)?;
                let store = self.new_value()?.store(val, var);
                self.add_insts(&[store])?;
            }
            if !self.symbol_table.insert(ident.clone(), Val::Var(var))? {
                return Err(anyhow!("redefinition of variable: {}", ident));
            }
        }

        Ok(())
    }

    fn codegen_stmt(&mut self, stmt: Stmt) -> Result<bool> {
        let val = match stmt {
            Stmt::Assign { lval, exp } => {
                let lval = match lval {
                    LVal { ident } => self.symbol_table.get(&ident)?,
                };
                let lval = match lval {
                    Val::Var(v) => v,
                    _ => return Err(anyhow!("assign to const variable")),
                };
                let val = self.codegen_exp(exp)?;
                let store = self.new_value()?.store(val, lval);
                self.add_insts(&[store])?;
                false
            }
            Stmt::Exp(exp) => {
                if let Some(exp) = exp {
                    let _ = self.codegen_exp(exp)?;
                }
                false
            }
            Stmt::Block(block) => self.codegen_block(block)?,
            Stmt::Ret(exp) => {
                let ret_val = match exp {
                    Some(exp) => Some(self.codegen_exp(exp)?),
                    None => None,
                };
                let ret = self.new_value()?.ret(ret_val);
                self.add_insts(&[ret])?;
                true
            }
            Stmt::IfElse { cond, then, else_ } => match else_ {
                Some(else_) => self.codegen_ifelse(cond, then, else_)?,
                None => self.codegen_if(cond, then)?,
            },
        };

        Ok(val)
    }

    fn codegen_if(&mut self, cond: Exp, then: Box<Stmt>) -> Result<bool> {
        let then_bb = self.new_bb()?;
        let end_bb = self.new_bb()?;

        let cond_val = self.codegen_exp(cond)?;
        if let Some(num) = self.take_const(cond_val)? {
            if num != 0 {
                return self.codegen_stmt(*then);
            } else {
                return Ok(false);
            }
        }

        self.add_bbs(&[then_bb, end_bb])?;
        let branch_inst = self.new_value()?.branch(cond_val, then_bb, end_bb);
        self.add_insts(&[branch_inst])?;

        self.bb = Some(then_bb);
        if !self.codegen_stmt(*then)? {
            let jump_inst = self.new_value()?.jump(end_bb);
            self.add_insts(&[jump_inst])?;
        }

        self.bb = Some(end_bb);
        Ok(false)
    }

    fn codegen_ifelse(&mut self, cond: Exp, then: Box<Stmt>, else_: Box<Stmt>) -> Result<bool> {
        let then_bb = self.new_bb()?;
        let else_bb = self.new_bb()?;
        let end_bb = self.new_bb()?;

        let cond_val = self.codegen_exp(cond)?;
        if let Some(num) = self.take_const(cond_val)? {
            if num != 0 {
                return self.codegen_stmt(*then);
            } else {
                return self.codegen_stmt(*else_);
            }
        }

        self.add_bbs(&[then_bb, else_bb, end_bb])?;
        let branch_inst = self.new_value()?.branch(cond_val, then_bb, else_bb);
        self.add_insts(&[branch_inst])?;

        self.bb = Some(then_bb);
        if !self.codegen_stmt(*then)? {
            let jump_inst = self.new_value()?.jump(end_bb);
            self.add_insts(&[jump_inst])?;
        }

        self.bb = Some(else_bb);
        if !self.codegen_stmt(*else_)? {
            let jump_inst = self.new_value()?.jump(end_bb);
            self.add_insts(&[jump_inst])?;
        }

        self.bb = Some(end_bb);
        Ok(false)
    }

    fn codegen_exp(&mut self, exp: Exp) -> Result<Value> {
        let Exp { exp } = exp;
        self.codegen_binary(exp)
    }

    fn codegen_binary(&mut self, exp: BinaryExp) -> Result<Value> {
        match exp {
            BinaryExp::Single(unary) => self.codegen_unary(unary),
            BinaryExp::Multi { op, lhs, rhs } => {
                match op {
                    crate::ast::BinaryOp::LAnd => return self.codegen_land(lhs, rhs),
                    crate::ast::BinaryOp::LOr => return self.codegen_lor(lhs, rhs),
                    _ => {}
                }

                let lhs_val = self.codegen_binary(*lhs)?;
                let rhs_val = self.codegen_binary(*rhs)?;

                if let Some(lhs_num) = self.take_const(lhs_val)? {
                    if let Some(rhs_num) = self.take_const(rhs_val)? {
                        let res = match op {
                            crate::ast::BinaryOp::Add => lhs_num + rhs_num,
                            crate::ast::BinaryOp::Sub => lhs_num - rhs_num,
                            crate::ast::BinaryOp::Mul => lhs_num * rhs_num,
                            crate::ast::BinaryOp::Div => lhs_num / rhs_num,
                            crate::ast::BinaryOp::Mod => lhs_num % rhs_num,
                            crate::ast::BinaryOp::Lt => (lhs_num < rhs_num) as Number,
                            crate::ast::BinaryOp::Le => (lhs_num <= rhs_num) as Number,
                            crate::ast::BinaryOp::Gt => (lhs_num > rhs_num) as Number,
                            crate::ast::BinaryOp::Ge => (lhs_num >= rhs_num) as Number,
                            crate::ast::BinaryOp::Eq => (lhs_num == rhs_num) as Number,
                            crate::ast::BinaryOp::Ne => (lhs_num != rhs_num) as Number,
                            _ => unreachable!(),
                        };
                        return Ok(self.new_value()?.integer(res));
                    }
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
                    _ => unreachable!(),
                };
                let val = self.new_value()?.binary(binary_op, lhs_val, rhs_val);
                self.add_insts(&[val])?;
                Ok(val)
            }
        }
    }

    fn codegen_land(&mut self, lhs: Box<BinaryExp>, rhs: Box<BinaryExp>) -> Result<Value> {
        let cond_val = self.codegen_binary(*lhs)?;
        if let Some(num) = self.take_const(cond_val)? {
            if num == 0 {
                return Ok(self.new_value()?.integer(0));
            } else {
                let val = self.codegen_binary(*rhs)?;
                if let Some(num) = self.take_const(val)? {
                    return Ok(self.new_value()?.integer((num != 0) as i32));
                } else {
                    return Ok(val);
                }
            }
        }

        let then_bb = self.new_bb()?;
        let end_bb = self.new_bb()?;

        let res = self.new_value()?.alloc(Type::get_i32());
        let init_val = self.new_value()?.integer(0);
        let init_res = self.new_value()?.store(init_val, res);
        let branch_inst = self.new_value()?.branch(cond_val, then_bb, end_bb);
        self.add_insts(&[res, init_res, branch_inst])?;

        self.add_bbs(&[then_bb])?;
        let previous_bb = self.bb()?;
        self.bb = Some(then_bb);
        let rhs_val = self.codegen_binary(*rhs)?;
        if let Some(num) = self.take_const(rhs_val)? {
            self.bb = Some(previous_bb);
            let insts = self
                .layout_mut()?
                .bb_mut(then_bb)
                .insts()
                .keys()
                .copied()
                .collect::<Vec<_>>();
            let insts_mut = self.layout_mut()?.bb_mut(previous_bb).insts_mut();
            let _ = insts_mut.pop_back().unwrap();
            let _ = insts_mut.pop_back().unwrap();
            let _ = insts_mut.pop_back().unwrap();
            insts_mut.extend(insts.into_iter());
            self.layout_mut()?.bbs_mut().remove(&then_bb);
            if num == 0 {
                return Ok(self.new_value()?.integer(0));
            } else {
                return Ok(cond_val);
            }
        }
        let zero = self.new_value()?.integer(0);
        let rhs_val = self.new_value()?.binary(BinaryOp::NotEq, rhs_val, zero);
        let assign_res = self.new_value()?.store(rhs_val, res);
        let jump_inst = self.new_value()?.jump(end_bb);
        self.add_insts(&[rhs_val, assign_res, jump_inst])?;

        self.add_bbs(&[end_bb])?;
        self.bb = Some(end_bb);
        let res = self.new_value()?.load(res);
        self.add_insts(&[res])?;
        Ok(res)
    }

    fn codegen_lor(&mut self, lhs: Box<BinaryExp>, rhs: Box<BinaryExp>) -> Result<Value> {
        let cond_val = self.codegen_binary(*lhs)?;
        if let Some(num) = self.take_const(cond_val)? {
            if num == 0 {
                let val = self.codegen_binary(*rhs)?;
                if let Some(num) = self.take_const(val)? {
                    return Ok(self.new_value()?.integer((num != 0) as i32));
                } else {
                    return Ok(val);
                }
            } else {
                return Ok(self.new_value()?.integer(1));
            }
        }

        let then_bb = self.new_bb()?;
        let end_bb = self.new_bb()?;

        let res = self.new_value()?.alloc(Type::get_i32());
        let init_val = self.new_value()?.integer(1);
        let init_res = self.new_value()?.store(init_val, res);
        let branch_inst = self.new_value()?.branch(cond_val, end_bb, then_bb);
        self.add_insts(&[res, init_res, branch_inst])?;

        self.add_bbs(&[then_bb])?;
        let previous_bb = self.bb()?;
        self.bb = Some(then_bb);
        let rhs_val = self.codegen_binary(*rhs)?;
        if let Some(num) = self.take_const(rhs_val)? {
            self.bb = Some(previous_bb);
            let insts = self
                .layout_mut()?
                .bb_mut(then_bb)
                .insts()
                .keys()
                .copied()
                .collect::<Vec<_>>();
            let insts_mut = self.layout_mut()?.bb_mut(previous_bb).insts_mut();
            let _ = insts_mut.pop_back().unwrap();
            let _ = insts_mut.pop_back().unwrap();
            let _ = insts_mut.pop_back().unwrap();
            insts_mut.extend(insts.into_iter());
            self.layout_mut()?.bbs_mut().remove(&then_bb);
            if num == 0 {
                return Ok(cond_val);
            } else {
                return Ok(self.new_value()?.integer(1 as i32));
            }
        }
        let zero = self.new_value()?.integer(0);
        let rhs_val = self.new_value()?.binary(BinaryOp::NotEq, rhs_val, zero);
        let assign_res = self.new_value()?.store(rhs_val, res);
        let jump_inst = self.new_value()?.jump(end_bb);
        self.add_insts(&[rhs_val, assign_res, jump_inst])?;

        self.add_bbs(&[end_bb])?;
        self.bb = Some(end_bb);
        let res = self.new_value()?.load(res);
        self.add_insts(&[res])?;

        Ok(res)
    }

    fn codegen_unary(&mut self, exp: UnaryExp) -> Result<Value> {
        let res = match exp {
            UnaryExp::Primary(primary) => self.codegen_primary(primary)?,
            UnaryExp::Unary { op, exp } => {
                let val = self.codegen_unary(*exp)?;
                match op {
                    UnaryOp::Pos => val,
                    UnaryOp::Neg => {
                        if let Some(num) = self.take_const(val)? {
                            return Ok(self.new_value()?.integer(-num));
                        }
                        let zero = self.new_value()?.integer(0);
                        let neg = self.new_value()?.binary(BinaryOp::Sub, zero, val);
                        self.add_insts(&[neg])?;
                        neg
                    }
                    UnaryOp::Not => {
                        if let Some(num) = self.take_const(val)? {
                            return Ok(self.new_value()?.integer((num == 0) as Number));
                        }
                        let zero = self.new_value()?.integer(0);
                        let not = self.new_value()?.binary(BinaryOp::Eq, zero, val);
                        self.add_insts(&[not])?;
                        not
                    }
                }
            }
        };
        Ok(res)
    }

    fn codegen_primary(&mut self, primary: PrimaryExp) -> Result<Value> {
        let res = match primary {
            PrimaryExp::Exp(exp) => self.codegen_exp(*exp)?,
            PrimaryExp::Num(num) => self.new_value()?.integer(num),
            PrimaryExp::LVal(lval) => {
                let ident = lval.ident;
                match self.symbol_table.get(&ident)? {
                    Val::Const(num) => self.new_value()?.integer(num),
                    Val::Var(var) => {
                        let val = self.new_value()?.load(var);
                        self.add_insts(&[val])?;
                        val
                    }
                }
            }
        };
        Ok(res)
    }
}

#[derive(Debug, Clone, Copy)]
enum Val {
    Const(Number),
    Var(Value),
}

type SymbolTable = HashMap<String, Val>;

struct ScopeSymbolTable {
    tables: Vec<SymbolTable>,
}

impl ScopeSymbolTable {
    fn new() -> Self {
        Self { tables: Vec::new() }
    }

    fn enter_scope(&mut self) {
        self.tables.push(HashMap::new());
    }

    fn exit_scope(&mut self) -> Result<()> {
        self.tables.pop().ok_or(anyhow!("no scope to exit"))?;
        Ok(())
    }

    fn insert(&mut self, ident: String, val: Val) -> Result<bool> {
        let current = self
            .tables
            .last_mut()
            .ok_or(anyhow!("no scope to insert"))?;
        let res = current.insert(ident, val).is_none();
        Ok(res)
    }

    fn get(&self, ident: &str) -> Result<Val> {
        for scope in self.tables.iter().rev() {
            if let Some(&v) = scope.get(ident) {
                return Ok(v);
            }
        }
        Err(anyhow!("undefined symbol: {}", ident))
    }
}
