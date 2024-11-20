use super::{expr::Expr, stat::Stats};
use crate::{
    codegen::{CodeGen, CodeGenError},
    ftable::FunctionInfo,
    ty::{Type, TypeCheckError, TypeInfo, Typed},
    vtable::VariableInfo,
};
use inkwell::{
    module::Linkage,
    types::{AnyTypeEnum, BasicMetadataTypeEnum},
    values::FunctionValue,
};

#[derive(Debug, Clone)]
pub struct Extern {
    pub ident: String,
    pub param_list: Option<Box<ParamList>>,
    pub ret: Type,
}

impl<'t> Typed<'t> for Extern {
    fn check(&self, scope: &'t mut crate::ty::TypeTable) -> Result<TypeInfo<'t>, TypeCheckError> {
        let params: Vec<_> = self.param_list.clone().map(Into::into).unwrap_or_default();
        let params = params.into_iter().map(|param| param.ty).collect::<Vec<_>>();

        if scope
            .bind_func(&self.ident, &params, self.ret.clone())
            .is_some()
        {
            Err(TypeCheckError::Redefinition(self.ident.clone()))
        } else {
            Ok(TypeInfo::Temporary(Type::Unit))
        }
    }
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
                    .to_llvm_type(gen.context(), gen.stable())
                    .unwrap()
                    .try_into()
                    .expect("Invalid function parameter type");
                ty
            })
            .collect::<Vec<_>>();

        let ret = match self
            .ret
            .clone()
            .to_llvm_type(gen.context(), gen.stable())
            .unwrap()
        {
            AnyTypeEnum::IntType(ty) => ty.fn_type(&param_types, false),
            AnyTypeEnum::FloatType(ty) => ty.fn_type(&param_types, false),
            _ => unimplemented!(),
        };

        let func_val =
            module_ref.add_function(&self.ident, ret, Some(Linkage::AvailableExternally));

        gen.ftable().bind(
            self.ident.clone(),
            FunctionInfo {
                ret: self.ret.clone(),
                params: params.into_iter().map(|p| p.ty).collect::<Vec<_>>(),
                func_val,
            },
        );

        Ok(func_val)
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
            params
                .iter()
                .map(|p| p.ty.clone())
                .collect::<Vec<_>>()
                .as_ref(),
            self.ret.clone(),
        );

        let mut local_scope = scope.enter();
        local_scope.clear_vars();

        for param in &params {
            local_scope.bind_var(&param.name, param.ty.clone());
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
                    .to_llvm_type(gen.context(), gen.stable())
                    .unwrap()
                    .try_into()
                    .expect("Invalid function parameter type");
                ty
            })
            .collect::<Vec<_>>();

        let ty = match self
            .ret
            .clone()
            .to_llvm_type(gen.context(), gen.stable())
            .unwrap()
        {
            AnyTypeEnum::IntType(ty) => ty.fn_type(&param_types, false),
            AnyTypeEnum::PointerType(ty) => ty.fn_type(&param_types, false),
            AnyTypeEnum::FloatType(ty) => ty.fn_type(&param_types, false),
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
                ret: self.ret.clone(),
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
