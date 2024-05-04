mod ast;
mod codegen;
mod riscv;

use anyhow::{anyhow, Result};
use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use riscv::RiscvGenerator;
use std::fs::{read_to_string, File};
use std::path::PathBuf;
use std::str::FromStr;

lalrpop_mod!(sysy);

fn main() -> Result<()> {
    let CmdArgs {
        mode,
        input,
        output,
    } = parse_cmd_args()?;

    let input = read_to_string(input)?;
    let mut output = File::create(output)?;

    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    let program = codegen::codegen(ast);

    match mode {
        Mode::Koopa => KoopaGenerator::new(&mut output).generate_on(&program)?,
        Mode::Riscv => RiscvGenerator::new(&mut output).generate_on(&program)?,
        Mode::Perf => todo!(),
    }

    Ok(())
}

enum Mode {
    Koopa,
    Riscv,
    Perf,
}

impl FromStr for Mode {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "-koopa" => Ok(Self::Koopa),
            "-riscv" => Ok(Self::Riscv),
            "-perf" => Ok(Self::Perf),
            _ => Err(anyhow!("unknown mode")),
        }
    }
}

struct CmdArgs {
    mode: Mode,
    input: PathBuf,
    output: PathBuf,
}

fn parse_cmd_args() -> Result<CmdArgs> {
    let mut args = std::env::args();
    args.next();
    let mode = args.next().ok_or(anyhow!("missing mode"))?.parse()?;
    let input = args.next().ok_or(anyhow!("missing input"))?.parse()?;
    args.next();
    let output = args.next().ok_or(anyhow!("missing output"))?.parse()?;
    Ok(CmdArgs {
        mode,
        input,
        output,
    })
}
