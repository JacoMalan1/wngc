use std::path::PathBuf;

use clap::Parser;

#[derive(Debug, Parser)]
#[command(version, about)]
pub struct Arguments {
    #[clap(long, short = 'R')]
    pub emit_llvm_ir: bool,

    #[clap(short = 'l')]
    pub link_libraries: Vec<String>,

    #[clap(short = 'L')]
    pub link_directories: Vec<PathBuf>,

    /// Source file path
    #[arg(value_name = "FILE")]
    pub file: PathBuf,
}
