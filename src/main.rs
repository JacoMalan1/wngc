#![warn(rust_2018_idioms, missing_debug_implementations, clippy::pedantic)]
#![allow(clippy::too_many_lines, clippy::module_name_repetitions)]

use clap::Parser;
use codegen::CodeGen;
use imc::Generator;
use inkwell::{
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    OptimizationLevel,
};
use lalrpop_util::lalrpop_mod;
use std::str::FromStr;
use ty::{TypeTable, Typed};

pub(crate) mod args;
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
    let args = args::Arguments::parse();

    Target::initialize_x86(&InitializationConfig::default());

    let src = std::fs::read_to_string(&args.file).unwrap();
    let name = args
        .file
        .file_stem()
        .expect("Failed to get file name")
        .to_string_lossy();

    match grammar::ProgParser::new().parse(&src) {
        Ok(val) => {
            if let Err(err) = val.check(&mut TypeTable::empty()) {
                println!("{err}");
                return;
            }

            let gen = Box::leak(Box::new(Generator::new()));
            val.codegen(gen, None).unwrap();
            let v = gen.module();
            let v_ref = v.as_ref().unwrap();
            let ir = v_ref.print_to_string();

            if args.emit_llvm_ir {
                std::fs::write(
                    std::path::PathBuf::from_str(&format!("{name}.ll")).unwrap(),
                    ir.to_bytes(),
                )
                .unwrap();
            }

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

            std::process::Command::new("yasm")
                .args(["-felf64", "-o", "main.o", "main.S"])
                .spawn()
                .unwrap()
                .wait()
                .unwrap();

            let mut linker_args = vec![
                format!("{name}.o"),
                "main.o".to_string(),
                "-o".to_string(),
                name.to_string(),
                "-lm".to_string(),
                "-lc".to_string(),
            ];

            args.link_directories
                .into_iter()
                .for_each(|ld| linker_args.push(format!("-L{}", ld.to_string_lossy())));

            args.link_libraries
                .into_iter()
                .for_each(|lib| linker_args.push(format!("-l{}", lib.trim())));

            std::process::Command::new("ld")
                .args(&linker_args)
                .spawn()
                .expect("Linking failed")
                .wait()
                .expect("Linking failed");

            std::fs::remove_file(format!("{name}.o")).expect("Failed to remove object file.");
            std::fs::remove_file("main.o").expect("Failed to remove object file.");
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
