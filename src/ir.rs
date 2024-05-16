use crate::ast::*;
use anyhow::{anyhow, Result};
use koopa::ir::{
    builder::{BasicBlockBuilder, GlobalInstBuilder, LocalBuilder, LocalInstBuilder, ValueBuilder},
    dfg::DataFlowGraph,
    layout::Layout,
    BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value, ValueKind,
};
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    static ref LIB_FUNCS: Vec<(&'static str, Vec<BType>, FuncType)> = vec![
        ("getint", vec![], FuncType::Int),
        ("getch", vec![], FuncType::Int),
        (
            "getarray",
            vec![BType::Pointer(Box::new(BType::Int))],
            FuncType::Int
        ),
        ("putint", vec![BType::Int], FuncType::Void),
        ("putch", vec![BType::Int], FuncType::Void),
        (
            "putarray",
            vec![BType::Int, BType::Pointer(Box::new(BType::Int))],
            FuncType::Void
        ),
        ("starttime", vec![], FuncType::Void),
        ("stoptime", vec![], FuncType::Void),
    ];
}

pub fn codegen(unit: CompUnit) -> Result<Program> {
    let mut program = Program::new();
    Context {
        program: &mut program,
        func: None,
        bb: None,
        loop_continue: None,
        loop_break: None,
        symbol_table: ScopeSymbolTable::new(),
        func_table: FuncTable::new(),
    }
    .codegen(unit)?;
    Ok(program)
}

struct Context<'a> {
    program: &'a mut Program,
    func: Option<(Function, Type)>,
    bb: Option<BasicBlock>,
    loop_continue: Option<BasicBlock>,
    loop_break: Option<BasicBlock>,
    symbol_table: ScopeSymbolTable,
    func_table: FuncTable,
}

impl<'a> Context<'a> {
    fn bb(&self) -> Result<BasicBlock> {
        self.bb.ok_or(anyhow!("Basic Block is not set in Context"))
    }

    fn func_mut(&mut self) -> Result<&mut FunctionData> {
        let func = self
            .func
            .as_ref()
            .ok_or(anyhow!(
                "Function is not set in Context. Maybe you called new_value in global scope."
            ))?
            .0;
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
            .extend(insts.iter().copied());
        Ok(())
    }

    fn add_bbs(&mut self, bbs: &[BasicBlock]) -> Result<()> {
        self.layout_mut()?.bbs_mut().extend(bbs.iter().copied());
        Ok(())
    }

    fn new_bb(&mut self) -> Result<BasicBlock> {
        Ok(self.dfg_mut()?.new_bb().basic_block(None))
    }

    fn new_value(&mut self) -> Result<LocalBuilder> {
        Ok(self.dfg_mut()?.new_value())
    }

    fn new_const_integer(&mut self, num: i32) -> Result<Value> {
        if self.func.is_none() {
            Ok(self.program.new_value().integer(num))
        } else {
            Ok(self.new_value()?.integer(num))
        }
    }

    fn take_const(&mut self, value: Value) -> Result<Option<i32>> {
        let res = if self.func.is_none() {
            match self.program.borrow_value(value).kind() {
                ValueKind::Integer(num) => Some(num.value()),
                _ => None,
            }
        } else {
            match self.dfg_mut()?.value(value).kind() {
                ValueKind::Integer(num) => Some(num.value()),
                _ => None,
            }
        };

        Ok(res)
    }

    fn merge_bb(&mut self, previous_bb: BasicBlock, drop_insts: usize) -> Result<()> {
        let current_bb = self.bb()?;
        self.bb = Some(previous_bb);
        let insts = self
            .layout_mut()?
            .bb_mut(current_bb)
            .insts()
            .keys()
            .copied()
            .collect::<Vec<_>>();
        let insts_mut = self.layout_mut()?.bb_mut(previous_bb).insts_mut();
        for _ in 0..drop_insts {
            let _ = insts_mut
                .pop_back()
                .ok_or(anyhow!("no instruction to drop"))?;
        }
        insts_mut.extend(insts);
        self.layout_mut()?.bbs_mut().remove(&current_bb);

        Ok(())
    }

    fn codegen(&mut self, unit: CompUnit) -> Result<()> {
        for &(func_name, ref params, ref func_type) in LIB_FUNCS.iter() {
            let ret_ty = map_func_type(func_type);
            let param_data = params.iter().map(map_btype).collect::<Vec<_>>();
            let func = self.program.new_func(FunctionData::new_decl(
                format!("@{}", func_name),
                param_data,
                ret_ty,
            ));
            if self
                .func_table
                .insert(func_name.to_string(), func)
                .is_some()
            {
                return Err(anyhow!("redefinition of function: {}", func_name));
            }
        }
        for item in unit.items {
            match item {
                CompUnitItem::Decl(decl) => self.codegen_global_decl(decl)?,
                CompUnitItem::FuncDef(func) => self.codegen_func(func)?,
            }
        }
        if !self.func_table.contains_key("main") {
            return Err(anyhow!("no main function"));
        }
        Ok(())
    }

    fn codegen_func(&mut self, func: FuncDef) -> Result<()> {
        let FuncDef {
            func_type,
            ident: func_name,
            params,
            block,
        } = func;

        let ret_ty = map_func_type(&func_type);
        let param_data = params
            .iter()
            .map(|p| (Some(format!("@{}", p.ident)), map_btype(&p.btype)))
            .collect::<Vec<_>>();
        let func = self.program.new_func(FunctionData::with_param_names(
            format!("@{}", func_name),
            param_data,
            ret_ty.clone(),
        ));
        if self.func_table.insert(func_name.clone(), func).is_some() {
            return Err(anyhow!("redefinition of function: {}", func_name));
        }

        self.func = Some((func, ret_ty.clone()));

        let entry = self.new_bb()?;
        self.add_bbs(&[entry])?;
        self.bb = Some(entry);

        self.symbol_table.enter_scope();
        for (i, param) in params.into_iter().enumerate() {
            let param_value = self.func_mut()?.params()[i];
            let alloc = self.new_value()?.alloc(map_btype(&param.btype));
            let store = self.new_value()?.store(param_value, alloc);
            self.add_insts(&[alloc, store])?;
            self.symbol_table.insert(param.ident, Val::Var(alloc))?;
        }
        if !self.codegen_block(block)? {
            if ret_ty.is_unit() {
                let ret = self.new_value()?.ret(None);
                self.add_insts(&[ret])?;
            } else {
                return Err(anyhow!("no return statement in function"));
            }
        }
        self.symbol_table.exit_scope()?;

        if func_name == "main" && !ret_ty.is_i32() {
            return Err(anyhow!("main function must return int"));
        }

        Ok(())
    }

    fn codegen_block(&mut self, block: Block) -> Result<bool> {
        let mut res = false;
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
        Ok(res)
    }

    fn codegen_global_decl(&mut self, decl: Decl) -> Result<()> {
        match decl {
            Decl::Const(const_decl) => self.codegen_global_const_decl(const_decl),
            Decl::Var(var_decl) => self.codegen_global_var_decl(var_decl),
        }
    }

    fn codegen_decl(&mut self, decl: Decl) -> Result<()> {
        match decl {
            Decl::Const(const_decl) => self.codegen_const_decl(const_decl),
            Decl::Var(var_decl) => self.codegen_var_decl(var_decl),
        }
    }

    fn codegen_global_const_decl(&mut self, const_decl: ConstDecl) -> Result<()> {
        let ConstDecl { btype, defs } = const_decl;
        let ty = map_btype(&btype);

        for def in defs {
            let ConstDef { ident, val } = def;
            let val = self.codegen_exp(val)?;
            let val_data = self.program.borrow_value(val);
            assert_eq!(ty, Type::get_i32());
            let num = match val_data.kind() {
                ValueKind::Integer(num) => num.value(),
                _ => return Err(anyhow!("non-integer constant expression: {}", ident)),
            };
            self.symbol_table.insert(ident.clone(), Val::Const(num))?;
        }

        Ok(())
    }

    fn codegen_const_decl(&mut self, const_decl: ConstDecl) -> Result<()> {
        let ConstDecl { btype, defs } = const_decl;
        let ty = map_btype(&btype);

        for def in defs {
            let ConstDef { ident, val } = def;
            let val = self.codegen_exp(val)?;
            let val_data = self.program.borrow_value(val);
            assert_eq!(ty, Type::get_i32());
            let num = match val_data.kind() {
                ValueKind::Integer(num) => num.value(),
                _ => return Err(anyhow!("non-integer constant expression: {}", ident)),
            };
            self.symbol_table.insert(ident.clone(), Val::Const(num))?;
        }

        Ok(())
    }

    fn codegen_global_var_decl(&mut self, var_decl: VarDecl) -> Result<()> {
        let VarDecl { btype, defs } = var_decl;
        let ty = map_btype(&btype);

        for def in defs {
            let VarDef { ident, val } = def;
            let init_val = match val {
                Some(exp) => {
                    let val = self.codegen_exp(exp)?;
                    let val_data = self.program.borrow_value(val);
                    if !val_data.kind().is_const() {
                        return Err(anyhow!("non-constant expression: {}", ident));
                    }
                    if val_data.ty() != &ty {
                        return Err(anyhow!("type mismatch for variable: {}", ident));
                    }
                    val
                }
                None => self.program.new_value().zero_init(ty.clone()),
            };
            let var = self.program.new_value().global_alloc(init_val);
            self.program
                .set_value_name(var, Some(format!("@{}", ident)));
            self.symbol_table.insert(ident.clone(), Val::Var(var))?;
        }

        Ok(())
    }

    fn codegen_var_decl(&mut self, var_decl: VarDecl) -> Result<()> {
        let VarDecl { btype, defs } = var_decl;
        let ty = map_btype(&btype);

        for def in defs {
            let VarDef { ident, val } = def;
            let var = self.new_value()?.alloc(ty.clone());
            self.add_insts(&[var])?;
            if let Some(exp) = val {
                let val = self.codegen_exp(exp)?;
                let store = self.new_value()?.store(val, var);
                self.add_insts(&[store])?;
            }
            self.symbol_table.insert(ident.clone(), Val::Var(var))?;
        }

        Ok(())
    }

    fn codegen_stmt(&mut self, stmt: Stmt) -> Result<bool> {
        let val = match stmt {
            Stmt::Assign { lval, exp } => {
                let LVal { ident } = lval;
                let lval = match self.symbol_table.get(&ident)? {
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
            Stmt::Block(block) => {
                self.symbol_table.enter_scope();
                let res = self.codegen_block(block)?;
                self.symbol_table.exit_scope()?;
                res
            }
            Stmt::Ret(exp) => {
                let expect_ret_ty = self
                    .func
                    .as_ref()
                    .ok_or(anyhow!("Function is not set in Context"))?
                    .1
                    .clone();
                let ret_val = match exp {
                    Some(exp) => {
                        let val = self.codegen_exp(exp)?;
                        let val_ty = self.dfg_mut()?.value(val).ty();
                        if expect_ret_ty != *val_ty {
                            return Err(anyhow!("return type mismatch"));
                        }
                        Some(val)
                    }
                    None => {
                        if !expect_ret_ty.is_unit() {
                            return Err(anyhow!("return type mismatch"));
                        }
                        None
                    }
                };
                let ret = self.new_value()?.ret(ret_val);
                self.add_insts(&[ret])?;
                true
            }
            Stmt::IfElse { cond, then, else_ } => match else_ {
                Some(else_) => self.codegen_ifelse(cond, *then, *else_)?,
                None => self.codegen_if(cond, *then)?,
            },
            Stmt::While { cond, body } => self.codegen_while(cond, *body)?,
            Stmt::Break => {
                let target = self
                    .loop_break
                    .ok_or(anyhow!("break statement outside loop"))?;
                let jump = self.new_value()?.jump(target);
                self.add_insts(&[jump])?;
                true
            }
            Stmt::Continue => {
                let target = self
                    .loop_continue
                    .ok_or(anyhow!("continue statement outside loop"))?;
                let jump = self.new_value()?.jump(target);
                self.add_insts(&[jump])?;
                true
            }
        };

        Ok(val)
    }

    fn codegen_if(&mut self, cond: Exp, then: Stmt) -> Result<bool> {
        let then_bb = self.new_bb()?;
        let end_bb = self.new_bb()?;

        let cond_val = self.codegen_exp(cond)?;
        if let Some(num) = self.take_const(cond_val)? {
            if num != 0 {
                return self.codegen_stmt(then);
            } else {
                return Ok(false);
            }
        }

        self.add_bbs(&[then_bb, end_bb])?;
        let branch_inst = self.new_value()?.branch(cond_val, then_bb, end_bb);
        self.add_insts(&[branch_inst])?;

        self.bb = Some(then_bb);
        if !self.codegen_stmt(then)? {
            let jump_inst = self.new_value()?.jump(end_bb);
            self.add_insts(&[jump_inst])?;
        }

        self.bb = Some(end_bb);
        Ok(false)
    }

    fn codegen_ifelse(&mut self, cond: Exp, then: Stmt, else_: Stmt) -> Result<bool> {
        let then_bb = self.new_bb()?;
        let else_bb = self.new_bb()?;
        let end_bb = self.new_bb()?;

        let cond_val = self.codegen_exp(cond)?;
        if let Some(num) = self.take_const(cond_val)? {
            if num != 0 {
                return self.codegen_stmt(then);
            } else {
                return self.codegen_stmt(else_);
            }
        }

        self.add_bbs(&[then_bb, else_bb, end_bb])?;
        let branch_inst = self.new_value()?.branch(cond_val, then_bb, else_bb);
        self.add_insts(&[branch_inst])?;

        self.bb = Some(then_bb);
        if !self.codegen_stmt(then)? {
            let jump_inst = self.new_value()?.jump(end_bb);
            self.add_insts(&[jump_inst])?;
        }

        self.bb = Some(else_bb);
        if !self.codegen_stmt(else_)? {
            let jump_inst = self.new_value()?.jump(end_bb);
            self.add_insts(&[jump_inst])?;
        }

        self.bb = Some(end_bb);
        Ok(false)
    }

    fn codegen_while(&mut self, cond: Exp, body: Stmt) -> Result<bool> {
        let cond_bb = self.new_bb()?;
        let body_bb = self.new_bb()?;
        let end_bb = self.new_bb()?;

        let jump_cond = self.new_value()?.jump(cond_bb);
        self.add_insts(&[jump_cond])?;

        let previous_bb = self.bb()?;
        self.add_bbs(&[cond_bb])?;
        self.bb = Some(cond_bb);
        let cond_val = self.codegen_exp(cond)?;
        if let Some(num) = self.take_const(cond_val)? {
            if num == 0 {
                self.merge_bb(previous_bb, 1)?;
                return Ok(false);
            } else {
                let previous_loop_break = self.loop_break;
                let previous_loop_continue = self.loop_continue;
                self.loop_break = Some(end_bb);
                self.loop_continue = Some(cond_bb);
                if !self.codegen_stmt(body)? {
                    let jump_cond = self.new_value()?.jump(cond_bb);
                    self.add_insts(&[jump_cond])?;
                }
                self.add_bbs(&[end_bb])?;
                self.bb = Some(end_bb);
                self.loop_break = previous_loop_break;
                self.loop_continue = previous_loop_continue;
                return Ok(false);
            }
        }

        let branch_inst = self.new_value()?.branch(cond_val, body_bb, end_bb);
        self.add_insts(&[branch_inst])?;

        self.add_bbs(&[body_bb])?;
        self.bb = Some(body_bb);
        let previous_loop_break = self.loop_break;
        let previous_loop_continue = self.loop_continue;
        self.loop_break = Some(end_bb);
        self.loop_continue = Some(cond_bb);
        if !self.codegen_stmt(body)? {
            let jump_cond = self.new_value()?.jump(cond_bb);
            self.add_insts(&[jump_cond])?;
        }

        self.add_bbs(&[end_bb])?;
        self.bb = Some(end_bb);
        self.loop_break = previous_loop_break;
        self.loop_continue = previous_loop_continue;
        Ok(false)
    }

    fn codegen_exp(&mut self, exp: Exp) -> Result<Value> {
        let Exp { exp } = exp;
        self.codegen_binary(exp)
    }

    fn codegen_binary(&mut self, exp: BinaryExp) -> Result<Value> {
        match exp {
            BinaryExp::Single(unary) => self.codegen_unary_exp(unary),
            BinaryExp::Multi { op, lhs, rhs } => {
                match op {
                    crate::ast::BinaryOp::LAnd => return self.codegen_land(*lhs, *rhs),
                    crate::ast::BinaryOp::LOr => return self.codegen_lor(*lhs, *rhs),
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
                        return self.new_const_integer(res);
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

    fn codegen_land(&mut self, lhs: BinaryExp, rhs: BinaryExp) -> Result<Value> {
        let cond_val = self.codegen_binary(lhs)?;
        if let Some(num) = self.take_const(cond_val)? {
            if num == 0 {
                return Ok(self.new_value()?.integer(0));
            } else {
                let val = self.codegen_binary(rhs)?;
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
        let rhs_val = self.codegen_binary(rhs)?;
        if let Some(num) = self.take_const(rhs_val)? {
            self.merge_bb(previous_bb, 3)?;
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

    fn codegen_lor(&mut self, lhs: BinaryExp, rhs: BinaryExp) -> Result<Value> {
        let cond_val = self.codegen_binary(lhs)?;
        if let Some(num) = self.take_const(cond_val)? {
            if num == 0 {
                let val = self.codegen_binary(rhs)?;
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
        let rhs_val = self.codegen_binary(rhs)?;
        if let Some(num) = self.take_const(rhs_val)? {
            self.merge_bb(previous_bb, 3)?;
            if num == 0 {
                return Ok(cond_val);
            } else {
                return Ok(self.new_value()?.integer(1));
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

    fn codegen_unary_exp(&mut self, exp: UnaryExp) -> Result<Value> {
        match exp {
            UnaryExp::Primary(primary) => self.codegen_primary(primary),
            UnaryExp::Unary { op, exp } => self.codegen_unary(op, *exp),
            UnaryExp::Call { ident, args } => self.codegen_call(ident, args),
        }
    }

    fn codegen_primary(&mut self, primary: PrimaryExp) -> Result<Value> {
        let res = match primary {
            PrimaryExp::Exp(exp) => self.codegen_exp(*exp)?,
            PrimaryExp::Num(num) => self.new_const_integer(num)?,
            PrimaryExp::LVal(lval) => {
                let ident = lval.ident;
                match self.symbol_table.get(&ident)? {
                    Val::Const(val) => self.new_const_integer(val)?,
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

    fn codegen_unary(&mut self, op: UnaryOp, exp: UnaryExp) -> Result<Value> {
        let val = self.codegen_unary_exp(exp)?;
        let res = match op {
            UnaryOp::Pos => val,
            UnaryOp::Neg => {
                if let Some(num) = self.take_const(val)? {
                    return self.new_const_integer(-num);
                }
                let zero = self.new_value()?.integer(0);
                let neg = self.new_value()?.binary(BinaryOp::Sub, zero, val);
                self.add_insts(&[neg])?;
                neg
            }
            UnaryOp::Not => {
                if let Some(num) = self.take_const(val)? {
                    return self.new_const_integer((num == 0) as Number);
                }
                let zero = self.new_value()?.integer(0);
                let not = self.new_value()?.binary(BinaryOp::Eq, zero, val);
                self.add_insts(&[not])?;
                not
            }
        };
        Ok(res)
    }

    fn codegen_call(&mut self, ident: String, args: Vec<Exp>) -> Result<Value> {
        let func = *self
            .func_table
            .get(&ident)
            .ok_or(anyhow!("undefined function: {}", ident))?;
        let args = args
            .into_iter()
            .map(|arg| self.codegen_exp(arg))
            .collect::<Result<Vec<_>>>()?;
        let call = self.new_value()?.call(func, args);
        self.add_insts(&[call])?;
        Ok(call)
    }
}

#[derive(Debug, Clone, Copy)]
enum Val {
    Const(i32),
    Var(Value),
}

type SymbolTable = HashMap<String, Val>;
type FuncTable = HashMap<String, Function>;

struct ScopeSymbolTable {
    tables: Vec<SymbolTable>,
}

impl ScopeSymbolTable {
    fn new() -> Self {
        Self {
            tables: vec![SymbolTable::new()],
        }
    }

    fn enter_scope(&mut self) {
        self.tables.push(SymbolTable::new());
    }

    fn exit_scope(&mut self) -> Result<()> {
        self.tables.pop().ok_or(anyhow!("no scope to exit"))?;
        Ok(())
    }

    fn insert(&mut self, ident: String, val: Val) -> Result<()> {
        let current = self
            .tables
            .last_mut()
            .ok_or(anyhow!("no scope to insert"))?;
        if current.insert(ident.clone(), val).is_some() {
            return Err(anyhow!("redefinition of symbol: {}", ident));
        }
        Ok(())
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

fn map_btype(btype: &BType) -> Type {
    match btype {
        BType::Int => Type::get_i32(),
        BType::Pointer(ty) => Type::get_pointer(map_btype(ty)),
    }
}

fn map_func_type(func_type: &FuncType) -> Type {
    match func_type {
        FuncType::Int => Type::get_i32(),
        FuncType::Void => Type::get_unit(),
    }
}
