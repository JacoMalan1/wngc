use crate::{
    codegen::CodeGen,
    ty::{Type, TypeInfo, Typed},
};
use agg::StructDecl;
use func::{Extern, FuncDecl};
use inkwell::values::FunctionValue;

pub mod agg;
pub mod expr;
pub mod func;
pub mod stat;

#[derive(Debug)]
pub struct Prog {
    pub items: Option<Box<Items>>,
}

impl<'t> Typed<'t> for Prog {
    fn check(
        &self,
        scope: &'t mut crate::ty::TypeTable,
    ) -> Result<crate::ty::TypeInfo<'t>, crate::ty::TypeCheckError> {
        scope.bind_func("sqrt", &[Type::Float], Type::Float);

        let items = self
            .items
            .clone()
            .map(|x| -> Vec<_> { x.into() })
            .unwrap_or_default();

        for item in items {
            item.check(scope)?;
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
        let (info, module) = crate::intrinsics::funcs::create_sqrt(gen.context(), module);
        gen.ftable().bind("sqrt", info);
        gen.set_module(Some(module));

        let v: Vec<_> = self.items.clone().map(Into::into).unwrap_or_default();

        for item in v {
            item.codegen(gen, builder)?;
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

#[derive(Debug, Clone)]
pub enum Item {
    Func(FuncDecl),
    Extern(Extern),
    Struct(StructDecl),
}

impl<'t> Typed<'t> for Item {
    fn check(
        &self,
        scope: &'t mut crate::ty::TypeTable,
    ) -> Result<TypeInfo<'t>, crate::ty::TypeCheckError> {
        match self {
            Self::Extern(ext) => ext.check(scope),
            Self::Func(f) => f.check(scope),
            Self::Struct(s) => s.check(scope),
        }
    }
}

impl<'ctx> CodeGen<'ctx> for Item {
    type Value = Option<FunctionValue<'ctx>>;

    fn codegen(
        &self,
        gen: &'ctx crate::imc::Generator<'ctx>,
        builder: Option<&inkwell::builder::Builder<'ctx>>,
    ) -> Result<Self::Value, crate::codegen::CodeGenError> {
        match self {
            Self::Func(func) => func.codegen(gen, builder).map(Some),
            Self::Extern(ext) => ext.codegen(gen, builder).map(Some),
            Self::Struct(s) => s.codegen(gen, builder).map(|()| None),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Items {
    pub item: Item,
    pub others: Option<Box<Items>>,
}

impl From<Box<Items>> for Vec<Item> {
    fn from(value: Box<Items>) -> Self {
        value
            .others
            .map(<Box<Items> as Into<Vec<Item>>>::into)
            .unwrap_or_default()
            .into_iter()
            .chain(std::iter::once(value.item))
            .collect()
    }
}
