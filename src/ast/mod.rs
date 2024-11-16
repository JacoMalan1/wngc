use crate::{
    codegen::CodeGen,
    ty::{Type, TypeInfo, Typed},
};
use func::FuncDecl;

pub mod expr;
pub mod func;

#[derive(Debug)]
pub struct Prog {
    pub funcs: Option<Box<Funcs>>,
}

impl<'t> Typed<'t> for Prog {
    fn check(
        &self,
        scope: &'t mut crate::ty::TypeTable,
    ) -> Result<crate::ty::TypeInfo<'t>, crate::ty::TypeCheckError> {
        let funcs = self
            .funcs
            .clone()
            .map(|x| -> Vec<_> { x.into() })
            .unwrap_or_default();

        for func in funcs {
            func.check(scope)?;
        }

        Ok(TypeInfo::Temporary(Type::Unit))
    }
}

impl<'ctx> CodeGen<'ctx> for Prog {
    type Value = ();

    fn codegen(
        &self,
        gen: &'ctx crate::imc::Generator<'ctx>,
        builder: Option<&inkwell::builder::Builder<'ctx>>,
    ) -> Result<Self::Value, crate::codegen::CodeGenError> {
        let module = gen.context().create_module("m1");
        let (info, module) = crate::intrinsics::funcs::create_printf(gen.context(), module);
        gen.ftable().bind("printf", info);
        gen.set_module(Some(module));

        let v: Vec<_> = self.funcs.clone().map(Into::into).unwrap_or_default();

        for func in v {
            func.codegen(gen, builder)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Funcs {
    pub func: FuncDecl,
    pub other: Option<Box<Funcs>>,
}

impl From<Box<Funcs>> for Vec<FuncDecl> {
    fn from(value: Box<Funcs>) -> Self {
        if let Some(other) = value.other {
            let mut res: Vec<_> = other.into();
            res.push(value.func);
            res
        } else {
            vec![value.func]
        }
    }
}
