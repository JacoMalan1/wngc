use inkwell::{
    context::Context,
    types::{AnyType, AnyTypeEnum},
    AddressSpace,
};
use std::{collections::HashMap, fmt::Display};
use thiserror::Error;

use crate::{ast::agg::Field, stable::StructTable};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    Str,
    Bool,
    Struct(String),
    Unit,
}

impl Type {
    pub fn to_llvm_type<'ctx>(
        &self,
        ctx: &'ctx Context,
        stable: &StructTable<'ctx>,
    ) -> Option<AnyTypeEnum<'ctx>> {
        match self {
            Self::Int => Some(ctx.i32_type().as_any_type_enum()),
            Self::Str => Some(ctx.ptr_type(AddressSpace::default()).as_any_type_enum()),
            Self::Unit => Some(ctx.void_type().as_any_type_enum()),
            Self::Bool => Some(ctx.bool_type().as_any_type_enum()),
            Self::Float => Some(ctx.f64_type().as_any_type_enum()),
            Self::Struct(name) => stable.lookup(name).map(|x| x.struct_ty.as_any_type_enum()),
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

#[derive(Debug, Clone)]
pub struct StructTypeInfo {
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, Default)]
pub struct TypeTable {
    funcs: HashMap<String, FuncTypeInfo>,
    vars: HashMap<String, VariableTypeInfo>,
    structs: HashMap<String, StructTypeInfo>,
}

#[derive(Debug, Clone)]
pub enum TypeInfo<'a> {
    Variable(&'a VariableTypeInfo),
    Temporary(Type),
    Function(&'a FuncTypeInfo),
    Struct(&'a StructTypeInfo),
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

    pub fn bind_struct(&mut self, ident: &str, fields: &[Field]) -> Option<StructTypeInfo> {
        self.structs.insert(
            ident.to_string(),
            StructTypeInfo {
                fields: Vec::from(fields),
            },
        )
    }

    pub fn lookup<'a>(&'a self, ident: &str) -> Option<TypeInfo<'a>> {
        self.vars
            .get(ident)
            .map(TypeInfo::Variable)
            .or(self.funcs.get(ident).map(TypeInfo::Function))
            .or(self.structs.get(ident).map(TypeInfo::Struct))
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
    #[error("Symbol `{0}` is already defined.")]
    Redefinition(String),
    #[error("Undefined type `{0}`.")]
    UndefinedType(String),
}

pub trait Typed<'t> {
    fn check(&self, scope: &'t mut TypeTable) -> Result<TypeInfo<'t>, TypeCheckError>;
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            match self {
                Self::Int => "Int",
                Self::Str => "String",
                Self::Unit => "()",
                Self::Bool => "Bool",
                Self::Float => "Float",
                Self::Struct(name) => name,
            }
        ))
    }
}
