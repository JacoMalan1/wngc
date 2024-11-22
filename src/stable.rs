use inkwell::types::StructType;
use std::{cell::RefCell, collections::HashMap};

use crate::ast::agg::Field;

#[derive(Debug)]
pub struct StructTable<'ctx> {
    structs: RefCell<HashMap<String, StructInfo<'ctx>>>,
}

#[derive(Debug, Clone)]
pub struct StructInfo<'ctx> {
    pub fields: Vec<Field>,
    pub struct_ty: StructType<'ctx>,
}

impl<'ctx> StructTable<'ctx> {
    pub fn empty() -> Self {
        Self {
            structs: RefCell::new(HashMap::new()),
        }
    }

    pub fn bind(&self, ident: &str, info: StructInfo<'ctx>) {
        self.structs.borrow_mut().insert(ident.to_string(), info);
    }

    pub fn lookup(&self, ident: &str) -> Option<StructInfo<'ctx>> {
        self.structs.borrow().get(ident).cloned()
    }
}
