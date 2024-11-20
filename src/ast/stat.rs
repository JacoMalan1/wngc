use super::expr::{Block, Cond, Expr, Lit};
use crate::{
    codegen::{CodeGen, CodeGenError},
    ftable::FunctionInfo,
    ty::{Type, TypeCheckError, TypeInfo, Typed},
    vtable::VariableInfo,
};
use inkwell::{
    types::BasicTypeEnum,
    values::{AnyValueEnum, BasicValue, BasicValueEnum},
};

#[derive(Debug, Clone)]
pub struct Stats {
    pub stat: Stat,
    pub other: Option<Box<Stats>>,
}

impl From<Box<Stats>> for Vec<Stat> {
    fn from(value: Box<Stats>) -> Self {
        if let Some(other) = value.other {
            let mut other = <Box<Stats> as Into<Vec<Stat>>>::into(other);
            other.push(value.stat);
            other
        } else {
            vec![value.stat]
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stat {
    VariableDecl {
        ident: String,
        type_specifier: Option<Type>,
        rhs: Box<Expr>,
    },
    Assign {
        lhs: String,
        rhs: Box<Expr>,
    },
    If {
        cond: Cond,
        block: Box<Block>,
    },
    IfElse {
        cond: Cond,
        true_block: Box<Block>,
        false_block: Box<Block>,
    },
    For {
        init: Box<Stat>,
        cond: Cond,
        inc: Box<Stat>,
        block: Box<Block>,
    },
    While {
        cond: Cond,
        block: Box<Block>,
    },
    Print(Box<Expr>),
    Return(Option<Box<Expr>>),
    None,
}

impl<'t> Typed<'t> for Stat {
    fn check(
        &self,
        scope: &'t mut crate::ty::TypeTable,
    ) -> Result<crate::ty::TypeInfo<'t>, crate::ty::TypeCheckError> {
        match self {
            Self::Assign { lhs, rhs } => {
                let info = scope.lookup(lhs);
                if info.is_none() {
                    return Err(TypeCheckError::UndefinedSymbol(lhs.clone()));
                }

                let lhs_ty = if let Some(TypeInfo::Variable(info)) = info {
                    info.ty.clone()
                } else {
                    return Err(TypeCheckError::InvalidOperation(
                        "Can only assign to variables.".to_string(),
                    ));
                };

                let rhs = match rhs.check(scope)? {
                    TypeInfo::Variable(info) => info.ty.clone(),
                    TypeInfo::Temporary(ty) => ty,
                    TypeInfo::Function(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can't assign function to a variable.".to_string(),
                        ))
                    }
                    TypeInfo::Struct(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can't assign struct to a variable.".to_string(),
                        ))
                    }
                };

                if lhs_ty == rhs {
                    Ok(TypeInfo::Temporary(Type::Unit))
                } else {
                    Err(TypeCheckError::TypeMismatch {
                        expected: lhs_ty,
                        found: rhs,
                    })
                }
            }
            Self::If { cond, block } => {
                cond.check(scope)?;
                let mut local = scope.enter();
                block.check(&mut local)?;
                Ok(TypeInfo::Temporary(Type::Unit))
            }
            Self::IfElse {
                cond,
                true_block,
                false_block,
            } => {
                cond.check(scope)?;
                let mut local = scope.enter();
                true_block.check(&mut local)?;
                let mut local = scope.enter();
                false_block.check(&mut local)?;
                Ok(TypeInfo::Temporary(Type::Unit))
            }
            Self::For {
                init,
                cond,
                inc,
                block,
            } => {
                init.check(scope)?;
                cond.check(scope)?;
                inc.check(scope)?;
                let mut local = scope.enter();
                block.check(&mut local)?;
                Ok(TypeInfo::Temporary(Type::Unit))
            }
            Self::While { cond, block } => {
                cond.check(scope)?;
                let mut local = scope.enter();
                block.check(&mut local)?;
                Ok(TypeInfo::Temporary(Type::Unit))
            }
            Self::Print(expr) => {
                let expr = expr.check(scope)?;
                if let TypeInfo::Function(_) = expr {
                    return Err(TypeCheckError::InvalidOperation(
                        "Can't print a function.".to_string(),
                    ));
                }

                Ok(TypeInfo::Temporary(Type::Unit))
            }
            Self::None => Ok(TypeInfo::Temporary(Type::Unit)),
            Self::Return(expr) => {
                if let Some(expr) = expr {
                    expr.check(scope)?;
                }
                Ok(TypeInfo::Temporary(Type::Unit))
            }
            Self::VariableDecl {
                ident,
                type_specifier,
                rhs,
            } => {
                let info = scope.lookup(ident);
                if matches!(info, Some(TypeInfo::Variable(_))) {
                    return Err(TypeCheckError::Redefinition(ident.clone()));
                }

                let rhs = rhs.check(scope)?;
                let rhs_ty = match rhs {
                    TypeInfo::Variable(info) => info.ty.clone(),
                    TypeInfo::Temporary(ty) => ty,
                    TypeInfo::Function(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Cannot assign function to variable.".to_string(),
                        ));
                    }
                    TypeInfo::Struct(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Cannot assign struct to variable.".to_string(),
                        ));
                    }
                };

                if let Some(ty) = type_specifier {
                    if let Type::Struct(name) = ty {
                        scope
                            .lookup(name)
                            .ok_or(TypeCheckError::UndefinedType(name.to_string()))?;
                    }

                    if rhs_ty != *ty {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: ty.clone(),
                            found: rhs_ty,
                        });
                    }
                }

                scope.bind_var(ident, rhs_ty);

                Ok(TypeInfo::Temporary(Type::Unit))
            }
        }
    }
}

impl<'ctx> CodeGen<'ctx> for Stat {
    type Value = Option<inkwell::values::AnyValueEnum<'ctx>>;

    fn codegen(
        &self,
        gen: &'ctx crate::imc::Generator<'ctx>,
        builder: Option<&inkwell::builder::Builder<'ctx>>,
    ) -> Result<Self::Value, crate::codegen::CodeGenError> {
        let builder = builder.ok_or(CodeGenError::NoBuilder)?;
        match self {
            Self::None => Ok(None),
            Self::Assign { lhs, rhs } => {
                let binding = gen.vtable().lookup(lhs);
                let rhs: BasicValueEnum<'_> = rhs.codegen(gen, Some(builder))?.try_into().unwrap();
                if let Some(binding) = binding {
                    builder.build_store(binding.val, rhs)?;
                    Ok(None)
                } else {
                    Err(CodeGenError::UndefinedVariable(lhs.clone()))
                }
            }
            Self::If { cond, block } => {
                let cond = cond.codegen(gen, Some(builder))?;
                let parent_func = builder
                    .get_insert_block()
                    .expect("Can't create an if statement outside of a block")
                    .get_parent()
                    .expect("Can't create an if statement outside of a function.");

                let true_block = gen.context().append_basic_block(parent_func, "");
                let true_builder = gen.context().create_builder();
                true_builder.position_at_end(true_block);
                block.codegen(gen, Some(&true_builder))?;

                let false_block = gen.context().append_basic_block(parent_func, "");
                let false_builder = gen.context().create_builder();
                false_builder.position_at_end(false_block);
                builder.build_conditional_branch(cond, true_block, false_block)?;
                builder.position_at_end(false_block);
                Ok(None)
            }
            Self::IfElse {
                cond,
                true_block,
                false_block,
            } => {
                let cond = cond.codegen(gen, Some(builder))?;
                let parent_func = builder
                    .get_insert_block()
                    .expect("Can't create an if statement outside of a block")
                    .get_parent()
                    .expect("Can't create an if statement outside of a function.");

                let tb = gen.context().append_basic_block(parent_func, "");
                let true_builder = gen.context().create_builder();
                true_builder.position_at_end(tb);
                true_block.codegen(gen, Some(&true_builder))?;

                let fb = gen.context().append_basic_block(parent_func, "");
                let false_builder = gen.context().create_builder();
                false_builder.position_at_end(fb);
                false_block.codegen(gen, Some(&false_builder))?;

                builder.build_conditional_branch(cond, tb, fb)?;

                let done = gen.context().append_basic_block(parent_func, "");
                true_builder.build_unconditional_branch(done)?;
                false_builder.build_unconditional_branch(done)?;

                builder.position_at_end(done);
                Ok(None)
            }
            Self::For {
                init,
                cond,
                inc,
                block,
            } => {
                init.codegen(gen, Some(builder))?;

                let parent_func = builder
                    .get_insert_block()
                    .expect("Can't create a for statement outside of a block")
                    .get_parent()
                    .expect("Can't create a for statement outside of a function.");

                let cond_block = gen.context().append_basic_block(parent_func, "");
                builder.build_unconditional_branch(cond_block)?;
                let cond_builder = gen.context().create_builder();
                cond_builder.position_at_end(cond_block);
                let cond = cond.codegen(gen, Some(&cond_builder))?;

                let body = gen.context().append_basic_block(parent_func, "");
                let done = gen.context().append_basic_block(parent_func, "");
                cond_builder.build_conditional_branch(cond, body, done)?;

                let body_builder = gen.context().create_builder();
                body_builder.position_at_end(body);

                block.codegen(gen, Some(&body_builder))?;
                inc.codegen(gen, Some(&body_builder))?;
                body_builder.build_unconditional_branch(cond_block)?;
                builder.position_at_end(done);

                Ok(None)
            }
            Self::While { cond, block } => {
                let parent_func = builder
                    .get_insert_block()
                    .expect("Can't create a while statement outside of a block")
                    .get_parent()
                    .expect("Can't create a while statement outside of a function.");

                let cond_block = gen.context().append_basic_block(parent_func, "");
                let cond_builder = gen.context().create_builder();

                builder.build_unconditional_branch(cond_block)?;

                cond_builder.position_at_end(cond_block);
                let cond = cond.codegen(gen, Some(&cond_builder))?;

                let body_block = gen.context().append_basic_block(parent_func, "");
                let done = gen.context().append_basic_block(parent_func, "");
                let body_builder = gen.context().create_builder();

                cond_builder.build_conditional_branch(cond, body_block, done)?;
                body_builder.position_at_end(body_block);

                block.codegen(gen, Some(&body_builder))?;
                body_builder.build_unconditional_branch(cond_block)?;

                builder.position_at_end(done);

                Ok(None)
            }
            Self::Return(expr) => {
                let val = if let Some(expr) = expr {
                    let val = expr.codegen(gen, Some(builder))?;
                    let val: Box<dyn BasicValue<'_>> = match val {
                        AnyValueEnum::IntValue(v) => Box::new(v),
                        AnyValueEnum::PointerValue(v) => Box::new(v),
                        AnyValueEnum::FloatValue(v) => Box::new(v),
                        AnyValueEnum::InstructionValue(_) => {
                            panic!("Can't return instruction from function")
                        }
                        _ => unreachable!(),
                    };
                    Some(val)
                } else {
                    None
                };

                builder.build_return(if let Some(ref v) = val {
                    Some(v.as_ref())
                } else {
                    None
                })?;
                Ok(None)
            }
            Self::Print(expr) => {
                let expr_val = expr.codegen(gen, Some(builder))?;
                let fmt = match &expr_val {
                    AnyValueEnum::IntValue(_) => "%d\n",
                    AnyValueEnum::FloatValue(_) => "%f\n",
                    AnyValueEnum::PointerValue(_) => "%s\n",
                    _ => panic!("Cannot print value."),
                };

                let fmt = Lit::Str(fmt.to_string())
                    .codegen(gen, Some(builder))?
                    .try_into()
                    .unwrap();

                let FunctionInfo { func_val, .. } = gen.ftable().lookup("printf").unwrap();
                builder.build_call(func_val, &[fmt, expr_val.try_into().unwrap()], "")?;

                Ok(None)
            }
            Self::VariableDecl { ident, rhs, .. } => {
                let rhs: BasicValueEnum<'_> = rhs.codegen(gen, Some(builder))?.try_into().unwrap();
                let ptr = builder.build_alloca(rhs.get_type(), "")?;
                builder.build_store(ptr, rhs)?;

                gen.vtable().bind(
                    ident,
                    VariableInfo {
                        ty: match rhs.get_type() {
                            BasicTypeEnum::IntType(ty) => {
                                if ty.get_bit_width() == 1 {
                                    Type::Bool
                                } else {
                                    Type::Int
                                }
                            }
                            BasicTypeEnum::FloatType(_) => Type::Float,
                            BasicTypeEnum::PointerType(_) => Type::Str,
                            _ => unreachable!(),
                        },
                        val: ptr,
                    },
                );
                Ok(None)
            }
        }
    }
}
