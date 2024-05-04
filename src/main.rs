mod ast;
mod codegen;

use anyhow::{anyhow, Result};
use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::fs::{read_to_string, File};
use std::path::PathBuf;

lalrpop_mod!(sysy);

fn main() -> Result<()> {
    let CmdArgs {
        mode,
        input,
        output,
    } = parse_cmd_args()?;

    assert!(mode == "-koopa", "unsupported mode");

    let input = read_to_string(input)?;
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    let program = codegen::codegen(ast);
    let mut output = File::create(output)?;
    KoopaGenerator::new(&mut output).generate_on(&program)?;

    Ok(())
}

struct CmdArgs {
    mode: String,
    input: PathBuf,
    output: PathBuf,
}

fn parse_cmd_args() -> Result<CmdArgs> {
    let mut args = std::env::args();
    args.next();
    let mode = args.next().ok_or(anyhow!("missing mode"))?;
    let input = args.next().ok_or(anyhow!("missing input"))?.parse()?;
    args.next();
    let output = args.next().ok_or(anyhow!("missing output"))?.parse()?;
    Ok(CmdArgs {
        mode,
        input,
        output,
    })
}
