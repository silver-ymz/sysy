#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

#[derive(Debug)]
pub enum Decl {
    Const(ConstDecl),
    Var(VarDecl),
}

#[derive(Debug)]
pub struct ConstDecl {
    pub btype: BType,
    pub defs: Vec<ConstDef>,
}

#[derive(Debug)]
pub struct VarDecl {
    pub btype: BType,
    pub defs: Vec<VarDef>,
}

#[derive(Debug)]
pub enum BType {
    Int,
}

#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub val: ConstExp,
}

#[derive(Debug)]
pub struct VarDef {
    pub ident: String,
    pub val: Option<Exp>,
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

#[derive(Debug)]
pub enum Stmt {
    Assign { lval: LVal, exp: Exp },
    Ret(Exp),
}

#[derive(Debug)]
pub struct Exp {
    pub exp: BinaryExp,
}

pub type ConstExp = Exp;

#[derive(Debug)]
pub struct LVal {
    pub ident: String,
}

#[derive(Debug)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    Num(Number),
    LVal(LVal),
}

pub type Number = i32;

#[derive(Debug)]
pub enum UnaryExp {
    Primary(PrimaryExp),
    Unary { op: UnaryOp, exp: Box<UnaryExp> },
}

#[derive(Debug)]
pub enum BinaryExp {
    Single(UnaryExp),
    Multi {
        op: BinaryOp,
        lhs: Box<BinaryExp>,
        rhs: Box<BinaryExp>,
    },
}

#[derive(Debug)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    LAnd,
    LOr,
}
