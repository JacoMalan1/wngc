use crate::ty::Type;
use std::{cell::RefCell, collections::HashMap};

#[derive(Debug, Clone)]
pub struct VariableInfo<'ctx> {
    pub ty: Type,
    pub val: inkwell::values::PointerValue<'ctx>,
}

#[derive(Debug, Clone)]
pub struct VariableTable<'ctx> {
    vars: RefCell<HashMap<String, VariableInfo<'ctx>>>,
}

impl<'ctx> VariableTable<'ctx> {
    pub fn empty() -> Self {
        Self {
            vars: RefCell::new(HashMap::new()),
        }
    }

    pub fn bind(
        &self,
        ident: impl AsRef<str>,
        info: VariableInfo<'ctx>,
    ) -> Option<VariableInfo<'ctx>> {
        self.vars
            .borrow_mut()
            .insert(ident.as_ref().to_owned(), info)
    }

    pub fn lookup(&self, ident: impl AsRef<str>) -> Option<VariableInfo<'_>> {
        self.vars.borrow().get(ident.as_ref()).cloned()
    }
}

impl Default for VariableTable<'_> {
    fn default() -> Self {
        Self::empty()
    }
}
