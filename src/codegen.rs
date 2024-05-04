use crate::ast::*;
use koopa::ir::{
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
    FunctionData, Program, Type,
};

pub fn codegen(unit: CompUnit) -> Program {
    let mut program = Program::new();
    let FuncDef {
        func_type,
        ident: func_name,
        block,
    } = unit.func_def;
    let Stmt { num } = block.stmt;

    let ret_ty = match func_type {
        FuncType::Int => Type::get_i32(),
    };
    let main = program.new_func(FunctionData::new(format!("@{}", func_name), vec![], ret_ty));
    let main = program.func_mut(main);

    let end = main
        .dfg_mut()
        .new_bb()
        .basic_block(Some("%end".to_string()));
    main.layout_mut().bbs_mut().extend([end]);

    let ret_val = main.dfg_mut().new_value().integer(num);
    let ret = main.dfg_mut().new_value().ret(Some(ret_val));
    main.layout_mut().bb_mut(end).insts_mut().extend([ret]);

    program
}
