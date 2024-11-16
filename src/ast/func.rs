use super::expr::{Block, Cond, Expr};
use crate::{
    ast::expr::{Exprs, Lit},
    codegen::{CodeGen, CodeGenError},
    ftable::FunctionInfo,
    ty::{Type, TypeCheckError, TypeInfo, Typed},
    vtable::VariableInfo,
};
use inkwell::{
    module::Linkage,
    types::{AnyTypeEnum, BasicMetadataTypeEnum},
    values::{AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue},
};

#[derive(Debug, Clone)]
pub struct Extern {
    pub ident: String,
    pub param_list: Option<Box<ParamList>>,
    pub ret: Type,
}

impl<'ctx> CodeGen<'ctx> for Extern {
    type Value = FunctionValue<'ctx>;

    fn codegen(
        &self,
        gen: &'ctx crate::imc::Generator<'ctx>,
        _: Option<&inkwell::builder::Builder<'ctx>>,
    ) -> Result<Self::Value, CodeGenError> {
        let module = gen.module();
        let module_ref = module.as_ref().ok_or(CodeGenError::NoModule)?;

        let params: Vec<_> = self.param_list.clone().map(Into::into).unwrap_or_default();
        let param_types = params
            .iter()
            .cloned()
            .map(|param| {
                let ty: BasicMetadataTypeEnum<'_> = param
                    .ty
                    .to_llvm_type(gen.context())
                    .try_into()
                    .expect("Invalid function parameter type");
                ty
            })
            .collect::<Vec<_>>();

        let ret = match self.ret.to_llvm_type(gen.context()) {
            AnyTypeEnum::IntType(ty) => ty.fn_type(&param_types, false),
            _ => unimplemented!(),
        };

        Ok(module_ref.add_function(&self.ident, ret, Some(Linkage::AvailableExternally)))
    }
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub ident: String,
    pub param_list: Option<Box<ParamList>>,
    pub body: FuncBody,
    pub ret: Type,
}

impl<'t> Typed<'t> for FuncDecl {
    fn check(&self, scope: &'t mut crate::ty::TypeTable) -> Result<TypeInfo<'t>, TypeCheckError> {
        let params = self
            .param_list
            .clone()
            .map(|x| -> Vec<_> { x.into() })
            .unwrap_or_default();

        scope.bind_func(
            &self.ident,
            params.iter().map(|p| p.ty).collect::<Vec<_>>().as_ref(),
            self.ret,
        );

        let mut local_scope = scope.enter();
        local_scope.clear_vars();

        for param in &params {
            local_scope.bind_var(&param.name, param.ty);
        }

        self.body.check(&mut local_scope)?;

        Ok(TypeInfo::Temporary(Type::Unit))
    }
}

impl<'ctx> CodeGen<'ctx> for FuncDecl {
    type Value = FunctionValue<'ctx>;

    fn codegen(
        &self,
        gen: &'ctx crate::imc::Generator<'ctx>,
        _: Option<&inkwell::builder::Builder<'ctx>>,
    ) -> Result<Self::Value, CodeGenError> {
        let m = gen.module();
        let module = m.as_ref().ok_or(CodeGenError::NoModule)?;
        let params = self
            .param_list
            .clone()
            .map(|list| -> Vec<_> { list.into() })
            .unwrap_or_default();
        let param_types = params
            .iter()
            .cloned()
            .map(|param| {
                let ty: BasicMetadataTypeEnum<'_> = param
                    .ty
                    .to_llvm_type(gen.context())
                    .try_into()
                    .expect("Invalid function parameter type");
                ty
            })
            .collect::<Vec<_>>();

        let ty = match self.ret.to_llvm_type(gen.context()) {
            AnyTypeEnum::IntType(ty) => ty.fn_type(&param_types, false),
            AnyTypeEnum::PointerType(ty) => ty.fn_type(&param_types, false),
            AnyTypeEnum::VoidType(ty) => ty.fn_type(&param_types, false),
            _ => unimplemented!(),
        };

        let fn_val = module.add_function(&self.ident, ty, None);

        let block = gen.context().append_basic_block(fn_val, "");
        let builder = gen.context().create_builder();
        builder.position_at_end(block);

        for (p_val, param) in fn_val.get_param_iter().zip(params) {
            let ptr = builder.build_alloca(p_val.get_type(), "")?;
            builder.build_store(ptr, p_val)?;
            gen.vtable().bind(
                param.name,
                VariableInfo {
                    ty: param.ty,
                    val: ptr,
                },
            );
        }

        gen.ftable().bind(
            self.ident.clone(),
            FunctionInfo {
                ret: self.ret,
                params: self
                    .param_list
                    .clone()
                    .map(|x| -> Vec<_> { x.into() })
                    .map(|x| x.into_iter().map(|y| y.ty).collect())
                    .unwrap_or_default(),
                func_val: fn_val,
            },
        );

        self.body.codegen(gen, Some(&builder))?;
        fn_val.verify(true);
        Ok(fn_val)
    }
}

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
                let lhs_ty = scope.lookup(lhs).map(|lhs| match lhs {
                    TypeInfo::Variable(info) => Ok(info.clone()),
                    _ => Err(TypeCheckError::InvalidOperation(
                        "Cannot assign to anything except a variable.".to_string(),
                    )),
                });

                let rhs = match rhs.check(scope)? {
                    TypeInfo::Variable(info) => info.ty,
                    TypeInfo::Temporary(ty) => ty,
                    TypeInfo::Function(_) => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can't assign function to a variable.".to_string(),
                        ))
                    }
                };

                if let Some(lhs) = lhs_ty {
                    let lhs = lhs?;
                    if rhs == lhs.ty {
                        Ok(TypeInfo::Temporary(Type::Unit))
                    } else {
                        Err(TypeCheckError::TypeMismatch {
                            expected: lhs.ty,
                            found: rhs,
                        })
                    }
                } else {
                    scope.bind_var(lhs, rhs);
                    Ok(TypeInfo::Temporary(Type::Unit))
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
                    let ptr = builder.build_alloca(rhs.get_type(), "")?;
                    builder.build_store(ptr, rhs)?;
                    gen.vtable().bind(
                        lhs,
                        VariableInfo {
                            ty: Type::Number,
                            val: ptr,
                        },
                    );
                    Ok(None)
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
            Self::Return(expr) => {
                let val = if let Some(expr) = expr {
                    let val = expr.codegen(gen, Some(builder))?;
                    let val: Box<dyn BasicValue<'_>> = match val {
                        AnyValueEnum::IntValue(v) => Box::new(v),
                        AnyValueEnum::PointerValue(v) => Box::new(v),
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
                    AnyValueEnum::PointerValue(_) => "%s\n",
                    _ => panic!("Cannot print value."),
                };

                let call = Expr::Call {
                    ident: "printf".to_string(),
                    args: Some(Box::new(Exprs {
                        expr: expr.clone(),
                        other: Some(Box::new(Exprs {
                            expr: Box::new(Expr::Lit(Lit::Str(fmt.to_string()))),
                            other: None,
                        })),
                    })),
                };
                call.codegen(gen, Some(builder))?;

                Ok(None)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParamList {
    pub param: Param,
    pub other: Option<Box<ParamList>>,
}

impl From<Box<ParamList>> for Vec<Param> {
    fn from(value: Box<ParamList>) -> Self {
        if let Some(other) = value.other {
            let mut other = <Box<ParamList> as Into<Vec<Param>>>::into(other);
            other.push(value.param);
            other
        } else {
            vec![value.param]
        }
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct FuncBody {
    pub stats: Option<Box<Stats>>,
    pub expr: Option<Box<Expr>>,
}

impl<'t> Typed<'t> for FuncBody {
    fn check(&self, scope: &'t mut crate::ty::TypeTable) -> Result<TypeInfo<'t>, TypeCheckError> {
        let stats = self
            .stats
            .clone()
            .map(|x| -> Vec<_> { x.into() })
            .unwrap_or_default();
        for stat in stats {
            stat.check(scope)?;
        }

        if let Some(ref expr) = self.expr {
            expr.check(scope)
        } else {
            Ok(TypeInfo::Temporary(Type::Unit))
        }
    }
}

impl<'ctx> CodeGen<'ctx> for FuncBody {
    type Value = ();

    fn codegen(
        &self,
        gen: &'ctx crate::imc::Generator<'ctx>,
        builder: Option<&inkwell::builder::Builder<'ctx>>,
    ) -> Result<Self::Value, CodeGenError> {
        let builder = builder.ok_or(CodeGenError::NoBuilder)?;
        if let Some(stats) = self.stats.clone() {
            let stats: Vec<_> = stats.into();
            for stat in stats {
                stat.codegen(gen, Some(builder))?;
            }
        }

        if let Some(expr) = self.expr.clone() {
            builder.build_return(Some(&expr.codegen(gen, Some(builder))?.into_int_value()))?;
        } else {
            builder.build_return(None)?;
        }

        Ok(())
    }
}
