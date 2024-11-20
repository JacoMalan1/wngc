#![warn(rust_2018_idioms, missing_debug_implementations, clippy::pedantic)]
#![allow(clippy::too_many_lines, clippy::module_name_repetitions)]

use codegen::CodeGen;
use imc::Generator;
use inkwell::{
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    OptimizationLevel,
};
use lalrpop_util::lalrpop_mod;
use std::str::FromStr;
use ty::{TypeTable, Typed};

pub(crate) mod ast;
pub(crate) mod codegen;
pub(crate) mod ftable;
pub(crate) mod imc;
pub(crate) mod intrinsics;
pub(crate) mod stable;
pub(crate) mod ty;
pub(crate) mod vtable;

lalrpop_mod!(
    #[allow(missing_debug_implementations, clippy::pedantic)]
    pub grammar
);

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        eprintln!("Usage: wngc <file>");
        return;
    }

    Target::initialize_x86(&InitializationConfig::default());

    let src = std::fs::read_to_string(&args[1]).unwrap();
    let name = &args[1].strip_suffix(".wng").unwrap().trim();
    match grammar::ProgParser::new().parse(&src) {
        Ok(val) => {
            val.check(&mut TypeTable::empty())
                .expect("Type checking failed");
            let gen = Box::leak(Box::new(Generator::new()));
            val.codegen(gen, None).unwrap();
            let v = gen.module();
            let v_ref = v.as_ref().unwrap();
            let ir = v_ref.print_to_string();
            std::fs::write(
                std::path::PathBuf::from_str(&format!("{name}.ll")).unwrap(),
                ir.to_bytes(),
            )
            .unwrap();
            let triple = TargetTriple::create("x86_64-unknown-linux-gnu");
            let target = Target::from_triple(&triple).unwrap();
            let machine = target
                .create_target_machine(
                    &triple,
                    "x86-64",
                    "+sse2,+avx2",
                    OptimizationLevel::Aggressive,
                    RelocMode::PIC,
                    CodeModel::Default,
                )
                .unwrap();
            machine
                .write_to_file(
                    v_ref,
                    FileType::Object,
                    &std::path::PathBuf::from_str(&format!("{name}.o")).unwrap(),
                )
                .unwrap();
            machine
                .write_to_file(
                    v_ref,
                    FileType::Assembly,
                    &std::path::PathBuf::from_str(&format!("{name}.S")).unwrap(),
                )
                .unwrap();

            std::process::Command::new("clang")
                .args(&[
                    "-O3".to_string(),
                    format!("{name}.o"),
                    format!("-o{name}"),
                    "-lm".to_string(),
                ])
                .spawn()
                .expect("Linking failed")
                .wait()
                .expect("Linking failed");
            std::fs::remove_file(format!("{name}.o")).expect("Failed to remove object file.");
        }
        Err(lalrpop_util::ParseError::InvalidToken { location }) => {
            let frag = &src[location..]
                .split_at(src[location..].find('\n').unwrap_or(src[location..].len()))
                .0;
            let cursor = format!("\x1b[31;1m{}\x1b[0m", "^".repeat(frag.len()));
            println!("\t{frag}\n\t{cursor}\nInvalid token at {location}.");
        }
        Err(err) => println!("{err}"),
    }
}
