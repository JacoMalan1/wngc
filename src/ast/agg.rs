use crate::{
    codegen::CodeGen,
    stable::StructInfo,
    ty::{Type, TypeCheckError, TypeInfo, Typed},
};
use inkwell::types::BasicTypeEnum;

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
            .into_iter()
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
        gen.stable().bind(&self.name, StructInfo { struct_ty: val });
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
