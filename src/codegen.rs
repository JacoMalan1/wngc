use crate::imc::Generator;
use inkwell::builder::Builder;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CodeGenError {
    #[error("No variable `{0}` exists.")]
    UndefinedVariable(String),
    #[error("No function `{0}` exists.")]
    UndefinedFunction(String),
    #[error("No struct `{0}` exists.")]
    UndefinedStruct(String),
    #[error("Type Error: Expected `{expected}`, found `{found}`")]
    Type { expected: String, found: String },
    #[error("{0}")]
    Builder(#[from] inkwell::builder::BuilderError),
    #[error("No builder provided")]
    NoBuilder,
    #[error("No active module")]
    NoModule,
}

pub trait CodeGen<'ctx> {
    type Value: 'ctx;
    fn codegen(
        &self,
        gen: &'ctx Generator<'ctx>,
        builder: Option<&Builder<'ctx>>,
    ) -> Result<Self::Value, CodeGenError>;
}
