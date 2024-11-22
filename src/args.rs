#![deny(missing_docs)]

use clap::Parser;
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(version)]
pub struct Arguments {
    /// Output LLVM Intermediate Code to <FILE>.ll
    #[clap(long, short = 'R')]
    pub emit_llvm_ir: bool,

    /// Additional linker libraries
    #[clap(short = 'l', value_name = "LIBRARY")]
    pub link_libraries: Vec<String>,

    /// Additional linker library directories
    #[clap(short = 'L', value_name = "DIRECTORY")]
    pub link_directories: Vec<PathBuf>,

    /// Source file path
    #[arg(value_name = "FILE")]
    pub file: PathBuf,
}
