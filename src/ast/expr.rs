use super::stat::Stats;
use crate::{
    codegen::{CodeGen, CodeGenError},
    ftable::FunctionInfo,
    ty::{Type, TypeCheckError, TypeInfo, Typed},
};
use inkwell::{
    builder::Builder,
    types::BasicTypeEnum,
    values::{AnyValue, AnyValueEnum, IntValue},
};

#[derive(Debug, Clone)]
pub struct Exprs {
    pub expr: Box<Expr>,
    pub other: Option<Box<Exprs>>,
}

impl From<Box<Exprs>> for Vec<Box<Expr>> {
    fn from(value: Box<Exprs>) -> Self {
        if let Some(other) = value.other {
            let mut res: Vec<_> = other.into();
            res.push(value.expr);
            res
        } else {
            vec![value.expr]
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Add {
        left: Box<Expr>,
        right: Box<Factor>,
    },
    Sub {
        left: Box<Expr>,
        right: Box<Factor>,
    },
    Call {
        ident: String,
        args: Option<Box<Exprs>>,
    },
    Cond(Cond),
    Lit(Lit),
}

impl<'t> Typed<'t> for Expr {
    fn check(&self, scope: &'t mut crate::ty::TypeTable) -> Result<TypeInfo<'t>, TypeCheckError> {
        match self {
            Self::Lit(lit) => lit.check(scope),
            Self::Add { left, right } | Self::Sub { left, right } => {
                let left = match left.check(scope)? {
                    TypeInfo::Variable(info) => info.ty.clone(),
                    TypeInfo::Temporary(ty) => ty,
                    TypeInfo::Function(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can't add or subtract functions.".to_string(),
                        ))
                    }
                    TypeInfo::Struct(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can't add or subtract structs.".to_string(),
                        ))
                    }
                };
                let right = match right.check(scope)? {
                    TypeInfo::Variable(info) => info.ty.clone(),
                    TypeInfo::Temporary(ty) => ty,
                    TypeInfo::Function(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can't add or subtract functions.".to_string(),
                        ))
                    }
                    TypeInfo::Struct(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can't add or subtract structs.".to_string(),
                        ))
                    }
                };

                if left == right {
                    Ok(TypeInfo::Temporary(left))
                } else {
                    Err(TypeCheckError::TypeMismatch {
                        expected: left,
                        found: right,
                    })
                }
            }
            Self::Call { ident, args } => {
                let sig = if let TypeInfo::Function(sig) = scope
                    .lookup(ident)
                    .ok_or(TypeCheckError::UndefinedSymbol(ident.clone()))?
                {
                    sig.clone()
                } else {
                    return Err(TypeCheckError::UndefinedSymbol(ident.clone()));
                };

                let args = args
                    .clone()
                    .map(|x| -> Vec<_> { x.into() })
                    .unwrap_or_default();

                for (arg, param) in args.into_iter().zip(sig.params.into_iter()) {
                    let arg = match arg.check(scope)? {
                        TypeInfo::Variable(info) => info.ty.clone(),
                        TypeInfo::Temporary(ty) => ty,
                        TypeInfo::Function(_) => {
                            return Err(TypeCheckError::InvalidOperation(
                                "Can't pass function as argument in call.".to_string(),
                            ))
                        }
                        TypeInfo::Struct(_) => {
                            return Err(TypeCheckError::InvalidOperation(
                                "Can't pass struct as argument in call.".to_string(),
                            ))
                        }
                    };
                    if arg != param {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: param,
                            found: arg,
                        });
                    }
                }

                Ok(TypeInfo::Temporary(sig.return_type))
            }
            Self::Cond(cond) => cond.check(scope),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Cond {
    Equals { left: Box<Expr>, right: Box<Expr> },
    NotEqual { left: Box<Expr>, right: Box<Expr> },
    Less { left: Box<Expr>, right: Box<Expr> },
    Greater { left: Box<Expr>, right: Box<Expr> },
    LessEqual { left: Box<Expr>, right: Box<Expr> },
    GreaterEqual { left: Box<Expr>, right: Box<Expr> },
    And { left: Box<Cond>, right: Box<Cond> },
    Or { left: Box<Cond>, right: Box<Cond> },
}

impl<'t> Typed<'t> for Cond {
    fn check(&self, scope: &'t mut crate::ty::TypeTable) -> Result<TypeInfo<'t>, TypeCheckError> {
        match self {
            Self::Equals { left, right }
            | Self::NotEqual { left, right }
            | Self::Less { left, right }
            | Self::Greater { left, right }
            | Self::LessEqual { left, right }
            | Self::GreaterEqual { left, right } => {
                let left = match left.check(scope)? {
                    TypeInfo::Variable(info) => info.ty.clone(),
                    TypeInfo::Temporary(ty) => ty,
                    TypeInfo::Function(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can't check use function itself in boolean operation.".to_string(),
                        ))
                    }
                    TypeInfo::Struct(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can't check use struct name in boolean operation.".to_string(),
                        ))
                    }
                };
                let right = match right.check(scope)? {
                    TypeInfo::Variable(info) => info.ty.clone(),
                    TypeInfo::Temporary(ty) => ty,
                    TypeInfo::Function(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can't check use function itself in boolean operation.".to_string(),
                        ))
                    }
                    TypeInfo::Struct(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can't check use struct name in boolean operation.".to_string(),
                        ))
                    }
                };

                if left == right {
                    Ok(TypeInfo::Temporary(left))
                } else {
                    Err(TypeCheckError::TypeMismatch {
                        expected: left,
                        found: right,
                    })
                }
            }
            Self::And { left, right } | Self::Or { left, right } => {
                left.check(scope)?;
                right.check(scope)?;
                Ok(TypeInfo::Temporary(Type::Bool))
            }
        }
    }
}

impl<'ctx> CodeGen<'ctx> for Cond {
    type Value = IntValue<'ctx>;

    fn codegen(
        &self,
        gen: &'ctx crate::imc::Generator<'ctx>,
        builder: Option<&Builder<'ctx>>,
    ) -> Result<Self::Value, CodeGenError> {
        let builder = builder.ok_or(CodeGenError::NoBuilder)?;
        match self {
            Self::Equals { left, right } => {
                let left = left.codegen(gen, Some(builder))?;
                let right = right.codegen(gen, Some(builder))?;

                if left.get_type() != right.get_type() {
                    return Err(CodeGenError::Type {
                        expected: left.get_type().to_string(),
                        found: right.get_type().to_string(),
                    });
                }

                Ok(builder.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    left.into_int_value(),
                    right.into_int_value(),
                    "",
                )?)
            }
            Self::NotEqual { left, right } => {
                let left = left.codegen(gen, Some(builder))?;
                let right = right.codegen(gen, Some(builder))?;

                if left.get_type() != right.get_type() {
                    return Err(CodeGenError::Type {
                        expected: left.get_type().to_string(),
                        found: right.get_type().to_string(),
                    });
                }

                Ok(builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    left.into_int_value(),
                    right.into_int_value(),
                    "",
                )?)
            }
            Self::Less { left, right } => {
                let left = left.codegen(gen, Some(builder))?;
                let right = right.codegen(gen, Some(builder))?;

                if left.get_type() != right.get_type() {
                    return Err(CodeGenError::Type {
                        expected: left.get_type().to_string(),
                        found: right.get_type().to_string(),
                    });
                }

                Ok(builder.build_int_compare(
                    inkwell::IntPredicate::SLT,
                    left.into_int_value(),
                    right.into_int_value(),
                    "",
                )?)
            }
            Self::LessEqual { left, right } => {
                let left = left.codegen(gen, Some(builder))?;
                let right = right.codegen(gen, Some(builder))?;

                if left.get_type() != right.get_type() {
                    return Err(CodeGenError::Type {
                        expected: left.get_type().to_string(),
                        found: right.get_type().to_string(),
                    });
                }

                Ok(builder.build_int_compare(
                    inkwell::IntPredicate::SLE,
                    left.into_int_value(),
                    right.into_int_value(),
                    "",
                )?)
            }
            Self::Greater { left, right } => {
                let left = left.codegen(gen, Some(builder))?;
                let right = right.codegen(gen, Some(builder))?;

                if left.get_type() != right.get_type() {
                    return Err(CodeGenError::Type {
                        expected: left.get_type().to_string(),
                        found: right.get_type().to_string(),
                    });
                }

                Ok(builder.build_int_compare(
                    inkwell::IntPredicate::SGT,
                    left.into_int_value(),
                    right.into_int_value(),
                    "",
                )?)
            }
            Self::GreaterEqual { left, right } => {
                let left = left.codegen(gen, Some(builder))?;
                let right = right.codegen(gen, Some(builder))?;

                if left.get_type() != right.get_type() {
                    return Err(CodeGenError::Type {
                        expected: left.get_type().to_string(),
                        found: right.get_type().to_string(),
                    });
                }

                Ok(builder.build_int_compare(
                    inkwell::IntPredicate::SGE,
                    left.into_int_value(),
                    right.into_int_value(),
                    "",
                )?)
            }
            Self::Or { left, right } => {
                let left = left.codegen(gen, Some(builder))?;
                let right = right.codegen(gen, Some(builder))?;

                Ok(builder.build_or(left, right, "")?)
            }
            Self::And { left, right } => {
                let left = left.codegen(gen, Some(builder))?;
                let right = right.codegen(gen, Some(builder))?;

                Ok(builder.build_and(left, right, "")?)
            }
        }
    }
}

impl<'ctx> CodeGen<'ctx> for Expr {
    type Value = AnyValueEnum<'ctx>;

    fn codegen(
        &self,
        gen: &'ctx crate::imc::Generator<'ctx>,
        builder: Option<&Builder<'ctx>>,
    ) -> Result<Self::Value, crate::codegen::CodeGenError> {
        let builder = builder.ok_or(CodeGenError::NoBuilder)?;
        match self {
            Self::Lit(lit) => lit.codegen(gen, Some(builder)),
            Self::Add { left, right } => {
                let left = left.codegen(gen, Some(builder))?;
                let right = right.codegen(gen, Some(builder))?;

                match (left, right) {
                    (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => {
                        Ok(builder.build_int_add(left, right, "")?.as_any_value_enum())
                    }
                    (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => {
                        Ok(builder
                            .build_float_add(left, right, "")?
                            .as_any_value_enum())
                    }
                    _ => unimplemented!(),
                }
            }
            Self::Sub { left, right } => {
                let left = left.codegen(gen, Some(builder))?;
                let right = right.codegen(gen, Some(builder))?;

                match (left, right) {
                    (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => {
                        Ok(builder.build_int_sub(left, right, "")?.as_any_value_enum())
                    }
                    (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => {
                        Ok(builder
                            .build_float_sub(left, right, "")?
                            .as_any_value_enum())
                    }
                    _ => unimplemented!(),
                }
            }
            Self::Call { ident, args } => {
                let FunctionInfo { func_val, .. } = gen
                    .ftable()
                    .lookup(ident)
                    .ok_or(CodeGenError::UndefinedFunction(ident.clone()))?;

                let args = args
                    .clone()
                    .map(|args| -> Vec<_> { args.into() })
                    .unwrap_or_default();

                let mut arg_vals = Vec::with_capacity(args.len());
                for arg in args {
                    arg_vals.push(
                        arg.codegen(gen, Some(builder))?
                            .try_into()
                            .expect("Value conversion error."),
                    );
                }

                Ok(builder
                    .build_call(func_val, &arg_vals, "")?
                    .as_any_value_enum())
            }
            Self::Cond(_) => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Factor {
    Mul {
        left: Box<Factor>,
        right: Lit,
    },
    Div {
        left: Box<Factor>,
        right: Lit,
    },
    Call {
        ident: String,
        args: Option<Box<Exprs>>,
    },
    Lit(Lit),
}

impl<'t> Typed<'t> for Factor {
    fn check(
        &self,
        scope: &'t mut crate::ty::TypeTable,
    ) -> Result<TypeInfo<'t>, crate::ty::TypeCheckError> {
        match self {
            Self::Lit(lit) => lit.check(scope),
            Self::Mul { left, right } | Self::Div { left, right } => {
                let left = match left.check(scope)? {
                    TypeInfo::Variable(info) => info.ty.clone(),
                    TypeInfo::Temporary(ty) => ty,
                    TypeInfo::Function(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can't multiply or divide a function.".to_string(),
                        ))
                    }
                    TypeInfo::Struct(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can't multiply or divide a struct.".to_string(),
                        ))
                    }
                };

                let right = match right.check(scope)? {
                    TypeInfo::Variable(info) => info.ty.clone(),
                    TypeInfo::Temporary(ty) => ty,
                    TypeInfo::Function(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can't multiply or divide by a function.".to_string(),
                        ));
                    }
                    TypeInfo::Struct(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can't multiply or divide by a struct.".to_string(),
                        ));
                    }
                };

                if left == right {
                    Ok(TypeInfo::Temporary(left))
                } else {
                    Err(TypeCheckError::TypeMismatch {
                        expected: left,
                        found: right,
                    })
                }
            }
            Self::Call { ident, .. } => match scope.lookup(ident) {
                Some(TypeInfo::Function(info)) => Ok(TypeInfo::Temporary(info.return_type.clone())),
                None => Err(TypeCheckError::UndefinedSymbol(ident.clone())),
                _ => Err(TypeCheckError::InvalidOperation(
                    "Only functions can be called".to_string(),
                )),
            },
        }
    }
}

impl<'ctx> CodeGen<'ctx> for Factor {
    type Value = AnyValueEnum<'ctx>;

    fn codegen(
        &self,
        gen: &'ctx crate::imc::Generator<'ctx>,
        builder: Option<&Builder<'ctx>>,
    ) -> Result<Self::Value, crate::codegen::CodeGenError> {
        let builder = builder.ok_or(CodeGenError::NoBuilder)?;
        match self {
            Self::Lit(lit) => lit.codegen(gen, Some(builder)),
            Self::Mul { left, right } => {
                let left = left.codegen(gen, Some(builder))?;
                let right = right.codegen(gen, Some(builder))?;

                match (left, right) {
                    (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => {
                        Ok(builder.build_int_mul(left, right, "")?.as_any_value_enum())
                    }
                    (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => {
                        Ok(builder
                            .build_float_mul(left, right, "")?
                            .as_any_value_enum())
                    }
                    _ => unimplemented!(),
                }
            }
            Self::Div { left, right } => {
                let left = left.codegen(gen, Some(builder))?;
                let right = right.codegen(gen, Some(builder))?;

                match (left, right) {
                    (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => Ok(builder
                        .build_int_signed_div(left, right, "")?
                        .as_any_value_enum()),
                    (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => {
                        Ok(builder
                            .build_float_div(left, right, "")?
                            .as_any_value_enum())
                    }
                    _ => unimplemented!(),
                }
            }
            Self::Call { ident, args } => Expr::Call {
                ident: ident.clone(),
                args: args.clone(),
            }
            .codegen(gen, Some(builder)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Lit {
    Num(i32),
    Float(f64),
    Bool(bool),
    Ident(String),
    Str(String),
}

impl<'t> Typed<'t> for Lit {
    fn check(
        &self,
        scope: &'t mut crate::ty::TypeTable,
    ) -> Result<crate::ty::TypeInfo<'t>, crate::ty::TypeCheckError> {
        match self {
            Self::Num(_) => Ok(TypeInfo::Temporary(Type::Int)),
            Self::Str(_) => Ok(TypeInfo::Temporary(Type::Str)),
            Self::Float(_) => Ok(TypeInfo::Temporary(Type::Float)),
            Self::Bool(_) => Ok(TypeInfo::Temporary(Type::Bool)),
            Self::Ident(id) => scope
                .lookup(id)
                .ok_or(crate::ty::TypeCheckError::UndefinedSymbol(id.clone())),
        }
    }
}

impl<'ctx> CodeGen<'ctx> for Lit {
    type Value = inkwell::values::AnyValueEnum<'ctx>;

    fn codegen(
        &self,
        gen: &'ctx crate::imc::Generator<'ctx>,
        builder: Option<&Builder<'ctx>>,
    ) -> Result<Self::Value, crate::codegen::CodeGenError>
    where
        Self::Value: inkwell::values::AnyValue<'ctx>,
    {
        match self {
            Self::Num(n) => Ok(gen
                .context()
                .i32_type()
                .const_int(
                    unsafe { std::mem::transmute::<i64, u64>(i64::from(*n)) },
                    false,
                )
                .as_any_value_enum()),
            Self::Float(f) => Ok(gen.context().f64_type().const_float(*f).as_any_value_enum()),
            Self::Ident(id) => {
                let builder = builder.ok_or(CodeGenError::NoBuilder)?;
                let binding = gen
                    .vtable()
                    .lookup(id)
                    .ok_or(CodeGenError::UndefinedVariable(id.clone()))?;

                let ty: BasicTypeEnum<'_> = binding
                    .ty
                    .to_llvm_type(gen.context(), gen.stable())
                    .unwrap()
                    .try_into()
                    .unwrap();

                Ok(builder.build_load(ty, binding.val, "")?.into())
            }
            Self::Str(s) => {
                let builder = builder.ok_or(CodeGenError::NoBuilder)?;
                let glob = builder.build_global_string_ptr(s, "")?;
                Ok(glob.as_any_value_enum())
            }
            Self::Bool(b) => Ok(gen
                .context()
                .bool_type()
                .const_int(u64::from(*b), false)
                .as_any_value_enum()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block(pub Option<Box<Stats>>);

impl<'t> Typed<'t> for Block {
    fn check(&self, scope: &'t mut crate::ty::TypeTable) -> Result<TypeInfo<'t>, TypeCheckError> {
        let stats = self
            .0
            .clone()
            .map(|x| -> Vec<_> { x.into() })
            .unwrap_or_default();

        for stat in stats {
            stat.check(scope)?;
        }

        Ok(TypeInfo::Temporary(Type::Unit))
    }
}

impl<'ctx> CodeGen<'ctx> for Block {
    type Value = ();

    fn codegen(
        &self,
        gen: &'ctx crate::imc::Generator<'ctx>,
        builder: Option<&Builder<'ctx>>,
    ) -> Result<Self::Value, CodeGenError> {
        let stats = self.0.clone();
        if let Some(stats) = stats {
            let stats: Vec<_> = stats.into();
            for s in stats {
                s.codegen(gen, builder)?;
            }
        }
        Ok(())
    }
}
