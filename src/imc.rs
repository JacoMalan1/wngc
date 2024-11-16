use crate::{ftable::FunctionTable, vtable::VariableTable};
use inkwell::module::Module;
use std::sync::{Mutex, MutexGuard};

pub struct Generator<'ctx> {
    ctx: inkwell::context::Context,
    vtable: VariableTable<'ctx>,
    ftable: FunctionTable<'ctx>,
    module: Mutex<Option<Module<'ctx>>>,
}

impl<'ctx> Generator<'ctx> {
    pub fn new() -> Self {
        Self {
            ctx: inkwell::context::Context::create(),
            vtable: VariableTable::empty(),
            ftable: FunctionTable::empty(),
            module: Mutex::new(None),
        }
    }

    pub fn context(&'ctx self) -> &'ctx inkwell::context::Context {
        &self.ctx
    }

    pub fn module(&'ctx self) -> MutexGuard<'_, Option<Module<'ctx>>> {
        self.module
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
    }

    pub fn set_module(&'ctx self, module: Option<Module<'ctx>>) {
        *self
            .module
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = module;
    }

    pub fn vtable(&'ctx self) -> &VariableTable<'ctx> {
        &self.vtable
    }

    pub fn ftable(&self) -> &FunctionTable<'ctx> {
        &self.ftable
    }
}
