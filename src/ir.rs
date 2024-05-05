use crate::ast::*;
use anyhow::{anyhow, Result};
use koopa::ir::{
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
    dfg::DataFlowGraph,
    layout::Layout,
    BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value,
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
        let bb = self
            .bb
            .ok_or(anyhow!("Basic Block is not set in Context"))?;
        self.layout_mut()?
            .bb_mut(bb)
            .insts_mut()
            .extend(insts.into_iter().copied());
        Ok(())
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

        let end = self.dfg_mut()?.new_bb().basic_block(None);
        self.layout_mut()?.bbs_mut().extend([end]);
        self.bb = Some(end);

        self.codegen_block(block)?;

        Ok(())
    }

    fn codegen_block(&mut self, block: Block) -> Result<()> {
        self.symbol_table.enter_scope();
        for item in block.items {
            match item {
                BlockItem::Decl(decl) => self.codegen_decl(decl)?,
                BlockItem::Stmt(stmt) => self.codegen_stmt(stmt)?,
            }
        }
        self.symbol_table.exit_scope()?;

        Ok(())
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
            let num = self.codegen_const_exp(val)?;
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
            let var = self.dfg_mut()?.new_value().alloc(ty.clone());
            self.add_insts(&[var])?;
            if let Some(exp) = val {
                let val = self.codegen_exp(exp)?;
                let store = self.dfg_mut()?.new_value().store(val, var);
                self.add_insts(&[store])?;
            }
            if !self.symbol_table.insert(ident.clone(), Val::Var(var))? {
                return Err(anyhow!("redefinition of variable: {}", ident));
            }
        }

        Ok(())
    }

    fn codegen_const_exp(&mut self, const_exp: ConstExp) -> Result<Number> {
        let Exp { exp } = const_exp;
        self.codegen_const_binary(exp)
    }

    fn codegen_stmt(&mut self, stmt: Stmt) -> Result<()> {
        match stmt {
            Stmt::Assign { lval, exp } => {
                let lval = match lval {
                    LVal { ident } => self.symbol_table.get(&ident)?,
                };
                let lval = match lval {
                    Val::Var(v) => v,
                    _ => return Err(anyhow!("assign to const variable")),
                };
                let val = self.codegen_exp(exp)?;
                let store = self.dfg_mut()?.new_value().store(val, lval);
                self.add_insts(&[store])?;
            }
            Stmt::Exp(exp) => {
                if let Some(exp) = exp {
                    let _ = self.codegen_exp(exp)?;
                }
            }
            Stmt::Block(block) => {
                self.codegen_block(block)?;
            }
            Stmt::Ret(exp) => {
                let ret_val = match exp {
                    Some(exp) => Some(self.codegen_exp(exp)?),
                    None => None,
                };
                let ret = self.dfg_mut()?.new_value().ret(ret_val);
                self.add_insts(&[ret])?;
            }
        }

        Ok(())
    }

    fn codegen_exp(&mut self, exp: Exp) -> Result<Value> {
        let Exp { exp } = exp;
        self.codegen_binary(exp)
    }

    fn codegen_const_binary(&mut self, exp: BinaryExp) -> Result<Number> {
        let res = match exp {
            BinaryExp::Single(unary) => self.codegen_const_unary(unary)?,
            BinaryExp::Multi { op, lhs, rhs } => {
                let lhs_val = self.codegen_const_binary(*lhs)?;
                let rhs_val = self.codegen_const_binary(*rhs)?;
                match op {
                    crate::ast::BinaryOp::Add => lhs_val + rhs_val,
                    crate::ast::BinaryOp::Sub => lhs_val - rhs_val,
                    crate::ast::BinaryOp::Mul => lhs_val * rhs_val,
                    crate::ast::BinaryOp::Div => lhs_val / rhs_val,
                    crate::ast::BinaryOp::Mod => lhs_val % rhs_val,
                    crate::ast::BinaryOp::Lt => (lhs_val < rhs_val) as Number,
                    crate::ast::BinaryOp::Le => (lhs_val <= rhs_val) as Number,
                    crate::ast::BinaryOp::Gt => (lhs_val > rhs_val) as Number,
                    crate::ast::BinaryOp::Ge => (lhs_val >= rhs_val) as Number,
                    crate::ast::BinaryOp::Eq => (lhs_val == rhs_val) as Number,
                    crate::ast::BinaryOp::Ne => (lhs_val != rhs_val) as Number,
                    crate::ast::BinaryOp::LAnd => (lhs_val != 0 && rhs_val != 0) as Number,
                    crate::ast::BinaryOp::LOr => (lhs_val != 0 || rhs_val != 0) as Number,
                }
            }
        };
        Ok(res)
    }

    fn codegen_binary(&mut self, exp: BinaryExp) -> Result<Value> {
        match exp {
            BinaryExp::Single(unary) => self.codegen_unary(unary),
            BinaryExp::Multi { op, lhs, rhs } => {
                let mut lhs_val = self.codegen_binary(*lhs)?;
                let mut rhs_val = self.codegen_binary(*rhs)?;
                if matches!(op, crate::ast::BinaryOp::LAnd | crate::ast::BinaryOp::LOr) {
                    let zero = self.dfg_mut()?.new_value().integer(0);
                    lhs_val = self
                        .dfg_mut()?
                        .new_value()
                        .binary(BinaryOp::NotEq, zero, lhs_val);
                    rhs_val = self
                        .dfg_mut()?
                        .new_value()
                        .binary(BinaryOp::NotEq, zero, rhs_val);
                    self.add_insts(&[lhs_val, rhs_val])?;
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
                    .dfg_mut()?
                    .new_value()
                    .binary(binary_op, lhs_val, rhs_val);
                self.add_insts(&[val])?;
                Ok(val)
            }
        }
    }

    fn codegen_const_unary(&mut self, exp: UnaryExp) -> Result<Number> {
        let res = match exp {
            UnaryExp::Primary(primary) => self.codegen_const_primary(primary)?,
            UnaryExp::Unary { op, exp } => {
                let val = self.codegen_const_unary(*exp)?;
                match op {
                    UnaryOp::Pos => val,
                    UnaryOp::Neg => -val,
                    UnaryOp::Not => (val == 0) as Number,
                }
            }
        };
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
                        let zero = self.dfg_mut()?.new_value().integer(0);
                        let neg = self.dfg_mut()?.new_value().binary(BinaryOp::Sub, zero, val);
                        self.add_insts(&[neg])?;
                        neg
                    }
                    UnaryOp::Not => {
                        let zero = self.dfg_mut()?.new_value().integer(0);
                        let not = self.dfg_mut()?.new_value().binary(BinaryOp::Eq, zero, val);
                        self.add_insts(&[not])?;
                        not
                    }
                }
            }
        };
        Ok(res)
    }

    fn codegen_const_primary(&mut self, primary: PrimaryExp) -> Result<Number> {
        let res = match primary {
            PrimaryExp::Exp(exp) => self.codegen_const_exp(*exp)?,
            PrimaryExp::Num(num) => num,
            PrimaryExp::LVal(lval) => {
                let ident = lval.ident;
                match self.symbol_table.get(&ident)? {
                    Val::Const(num) => num,
                    Val::Var(_) => {
                        return Err(anyhow!("variable in constant expression: {}", ident))
                    }
                }
            }
        };
        Ok(res)
    }

    fn codegen_primary(&mut self, primary: PrimaryExp) -> Result<Value> {
        let res = match primary {
            PrimaryExp::Exp(exp) => self.codegen_exp(*exp)?,
            PrimaryExp::Num(num) => self.dfg_mut()?.new_value().integer(num),
            PrimaryExp::LVal(lval) => {
                let ident = lval.ident;
                match self.symbol_table.get(&ident)? {
                    Val::Const(num) => self.dfg_mut()?.new_value().integer(num),
                    Val::Var(var) => {
                        let val = self.dfg_mut()?.new_value().load(var);
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
