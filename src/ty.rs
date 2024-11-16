use inkwell::{
    context::Context,
    types::{AnyType, AnyTypeEnum},
    AddressSpace,
};
use std::{collections::HashMap, fmt::Display};
use thiserror::Error;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    Number,
    Str,
    Bool,
    Unit,
}

impl Type {
    pub fn to_llvm_type(self, ctx: &Context) -> AnyTypeEnum<'_> {
        match self {
            Self::Number => ctx.i32_type().as_any_type_enum(),
            Self::Str => ctx.ptr_type(AddressSpace::default()).as_any_type_enum(),
            Self::Unit => ctx.void_type().as_any_type_enum(),
            Self::Bool => ctx.bool_type().as_any_type_enum(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariableTypeInfo {
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct FuncTypeInfo {
    pub params: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone, Default)]
pub struct TypeTable {
    funcs: HashMap<String, FuncTypeInfo>,
    vars: HashMap<String, VariableTypeInfo>,
}

#[derive(Debug, Clone)]
pub enum TypeInfo<'a> {
    Variable(&'a VariableTypeInfo),
    Temporary(Type),
    Function(&'a FuncTypeInfo),
}

impl TypeTable {
    pub fn empty() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn clear_vars(&mut self) {
        self.vars.clear();
    }

    pub fn bind_var(&mut self, ident: &str, ty: Type) -> Option<VariableTypeInfo> {
        self.vars.insert(ident.to_string(), VariableTypeInfo { ty })
    }

    pub fn bind_func(
        &mut self,
        ident: &str,
        params: &[Type],
        return_type: Type,
    ) -> Option<FuncTypeInfo> {
        self.funcs.insert(
            ident.to_string(),
            FuncTypeInfo {
                params: Vec::from(params),
                return_type,
            },
        )
    }

    pub fn lookup<'a>(&'a self, ident: &str) -> Option<TypeInfo<'a>> {
        self.vars
            .get(ident)
            .map(TypeInfo::Variable)
            .or(self.funcs.get(ident).map(TypeInfo::Function))
    }

    pub fn enter(&self) -> Self {
        self.clone()
    }
}

#[derive(Debug, Error)]
pub enum TypeCheckError {
    #[error("TypeError: Expected `{expected}`, found `{found}`")]
    TypeMismatch { expected: Type, found: Type },
    #[error("Undefined symbol `{0}`.")]
    UndefinedSymbol(String),
    #[error("Invalid operation: {0}")]
    InvalidOperation(String),
}

pub trait Typed<'t> {
    fn check(&self, scope: &'t mut TypeTable) -> Result<TypeInfo<'t>, TypeCheckError>;
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            match self {
                Self::Number => "Number",
                Self::Str => "String",
                Self::Unit => "()",
                Self::Bool => "Bool",
            }
        ))
    }
}
