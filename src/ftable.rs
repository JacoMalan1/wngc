use crate::ty::Type;
use inkwell::values::FunctionValue;
use std::{cell::RefCell, collections::HashMap};

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct FunctionInfo<'ctx> {
    pub ret: Type,
    pub params: Vec<Type>,
    pub func_val: FunctionValue<'ctx>,
}

#[derive(Debug, Clone)]
pub struct FunctionTable<'ctx> {
    sigs: RefCell<HashMap<String, FunctionInfo<'ctx>>>,
}

impl<'ctx> FunctionTable<'ctx> {
    pub fn empty() -> Self {
        Self {
            sigs: RefCell::new(HashMap::new()),
        }
    }

    pub fn bind(
        &self,
        ident: impl AsRef<str>,
        info: FunctionInfo<'ctx>,
    ) -> Option<FunctionInfo<'ctx>> {
        self.sigs
            .borrow_mut()
            .insert(ident.as_ref().to_owned(), info)
    }

    pub fn lookup(&'ctx self, ident: impl AsRef<str>) -> Option<FunctionInfo<'ctx>> {
        self.sigs.borrow().get(ident.as_ref()).cloned()
    }
}
