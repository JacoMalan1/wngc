use crate::{ftable::FunctionInfo, ty::Type};
use inkwell::{context::Context, module::Module, AddressSpace};

pub fn create_printf<'ctx>(
    ctx: &'ctx Context,
    module: Module<'ctx>,
) -> (FunctionInfo<'ctx>, Module<'ctx>) {
    let ty = ctx
        .void_type()
        .fn_type(&[ctx.ptr_type(AddressSpace::default()).into()], true);

    let fn_val = module.add_function("printf", ty, None);
    (
        FunctionInfo {
            params: vec![Type::Str],
            ret: Type::Unit,
            func_val: fn_val,
        },
        module,
    )
}
