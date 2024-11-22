use crate::{
    codegen::{CodeGen, CodeGenError},
    stable::StructInfo,
    ty::{Type, TypeCheckError, TypeInfo, Typed},
};
use inkwell::{types::BasicTypeEnum, values::BasicValueEnum};

use super::expr::Expr;

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub name: String,
    pub fields: Option<Box<Fields>>,
}

impl<'t> Typed<'t> for StructDecl {
    fn check(
        &self,
        scope: &'t mut crate::ty::TypeTable,
    ) -> Result<crate::ty::TypeInfo<'t>, crate::ty::TypeCheckError> {
        let fields: Vec<_> = self.fields.clone().map(Into::into).unwrap_or_default();
        if scope.bind_struct(&self.name, &fields).is_some() {
            return Err(TypeCheckError::Redefinition(self.name.clone()));
        }
        Ok(TypeInfo::Temporary(Type::Unit))
    }
}

impl<'ctx> CodeGen<'ctx> for StructDecl {
    type Value = ();

    fn codegen(
        &self,
        gen: &'ctx crate::imc::Generator<'ctx>,
        _: Option<&inkwell::builder::Builder<'ctx>>,
    ) -> Result<Self::Value, crate::codegen::CodeGenError> {
        let fields: Vec<_> = self.fields.clone().map(Into::into).unwrap_or_default();
        let types: Vec<BasicTypeEnum<'ctx>> = fields
            .iter()
            .map(|field| {
                field
                    .ty
                    .to_llvm_type(gen.context(), gen.stable())
                    .unwrap()
                    .try_into()
                    .unwrap()
            })
            .collect::<Vec<_>>();

        let val = gen.context().struct_type(&types, false);
        gen.stable().bind(
            &self.name,
            StructInfo {
                fields,
                struct_ty: val,
            },
        );
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Fields {
    pub field: Field,
    pub other: Option<Box<Fields>>,
}

impl From<Box<Fields>> for Vec<Field> {
    fn from(value: Box<Fields>) -> Self {
        value
            .other
            .map(|x| -> Vec<_> { x.into() })
            .unwrap_or_default()
            .into_iter()
            .chain(std::iter::once(value.field))
            .collect()
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct StructLit {
    pub name: String,
    pub args: Option<Box<FieldArgs>>,
}

impl<'t> Typed<'t> for StructLit {
    fn check(&self, scope: &'t mut crate::ty::TypeTable) -> Result<TypeInfo<'t>, TypeCheckError> {
        let info = scope
            .lookup(&self.name)
            .ok_or(TypeCheckError::UndefinedType(self.name.clone()))?;

        if let TypeInfo::Struct(info) = info {
            let args: Vec<_> = self.args.clone().map(Into::into).unwrap_or_default();

            if args.len() != info.fields.len() {
                return Err(TypeCheckError::WrongFieldCount {
                    name: self.name.clone(),
                    expected: info.fields.len(),
                    found: args.len(),
                });
            }

            for (arg, field) in args.into_iter().zip(info.fields.clone().into_iter()) {
                let arg = arg.expr.check(scope)?;
                let arg_ty = match arg {
                    TypeInfo::Variable(info) => info.ty.clone(),
                    TypeInfo::Temporary(ty) => ty,
                    _ => {
                        return Err(TypeCheckError::InvalidOperation(
                            "Can only assign variables and temporaries to struct fields."
                                .to_string(),
                        ));
                    }
                };
                if arg_ty != field.ty {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: field.ty.clone(),
                        found: arg_ty,
                    });
                }
            }

            Ok(TypeInfo::Temporary(Type::Struct(self.name.clone())))
        } else {
            Err(TypeCheckError::InvalidOperation(format!(
                "`{}` does not name a struct.",
                &self.name
            )))
        }
    }
}

impl<'ctx> CodeGen<'ctx> for StructLit {
    type Value = BasicValueEnum<'ctx>;

    fn codegen(
        &self,
        gen: &'ctx crate::imc::Generator<'ctx>,
        builder: Option<&inkwell::builder::Builder<'ctx>>,
    ) -> Result<Self::Value, crate::codegen::CodeGenError> {
        let builder = builder.ok_or(CodeGenError::NoBuilder)?;

        let info = gen
            .stable()
            .lookup(&self.name)
            .ok_or(CodeGenError::UndefinedStruct(self.name.clone()))?;

        let ptr = builder.build_alloca(info.struct_ty, "")?;

        let args: Vec<_> = self.args.clone().map(Into::into).unwrap_or_default();

        for (i, arg) in args.into_iter().enumerate() {
            let field_ptr = builder.build_struct_gep(
                info.struct_ty,
                ptr,
                u32::try_from(i).expect("Field index too large to fit u32."),
                "",
            )?;

            let arg: BasicValueEnum<'_> = arg.expr.codegen(gen, Some(builder))?.try_into().unwrap();
            builder.build_store(field_ptr, arg)?;
        }

        Ok(builder.build_load(info.struct_ty, ptr, "")?)
    }
}

#[derive(Debug, Clone)]
pub struct FieldArgs {
    pub others: Option<Box<FieldArgs>>,
    pub arg: FieldArg,
}

impl From<Box<FieldArgs>> for Vec<FieldArg> {
    fn from(value: Box<FieldArgs>) -> Self {
        value
            .others
            .map(|x| -> Vec<_> { x.into() })
            .unwrap_or_default()
            .into_iter()
            .chain(std::iter::once(value.arg))
            .collect()
    }
}

#[derive(Debug, Clone)]
pub struct FieldArg {
    pub field_name: String,
    pub expr: Box<Expr>,
}
