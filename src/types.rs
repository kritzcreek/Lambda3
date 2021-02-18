use crate::ast;
use crate::ast::Ty::Func;
use crate::cst::{self, ExprKind, TypeKind};
use crate::types::TypeErr::{TypeMismatch, UnknownVar};
use rowan::TextRange;
use std::collections::HashMap;
use std::hint::unreachable_unchecked;
use std::rc::Rc;

pub enum TypeErr {
    UnknownVar { var: String, span: TextRange },
    TypeMismatch { expected: ast::Ty, actual: ast::Ty },
}

struct Env {
    values: HashMap<String, (Rc<ast::Ty>, TextRange)>,
    types: HashMap<String, TextRange>,
}

fn infer(env: Env, expr: cst::Expr) -> Result<ast::Expr, TypeErr> {
    match expr.kind() {
        ExprKind::Var(var) => {
            let range = var.0.text_range();
            let ident = var.0.text().to_string();
            match env.values.get(&ident) {
                None => Err(UnknownVar {
                    var: ident,
                    span: range,
                }),
                Some((ty, _)) => Ok(ast::Expr::Var {
                    ident,
                    range,
                    ty: Rc::clone(ty),
                }),
            }
        }
        ExprKind::Lambda(lambda) => {
            let range = lambda.0.text_range();
            let binder = lambda.binder().unwrap();
            let body = lambda.body().unwrap();
            //ast::Expr::Lambda { range, ty: binder.ty(), body: , binder: }

            unreachable!()
        }
        ExprKind::Application(_) => unreachable!(),
        ExprKind::BooleanLit(lit) => {
            let range = lit.0.text_range();
            Ok(ast::Expr::Lit {
                range,
                ty: Rc::new(ast::Ty::Bool { range }),
                lit: ast::Lit::Bool(lit.bool()),
            })
        }
        ExprKind::IntLit(lit) => {
            let range = lit.0.text_range();
            Ok(ast::Expr::Lit {
                range,
                ty: Rc::new(ast::Ty::Int { range }),
                lit: ast::Lit::Int(lit.int()),
            })
        }
    }
}

fn check_type(env: &Env, ty: cst::Type) -> Result<ast::Ty, TypeErr> {
    let ty = match ty.kind() {
        TypeKind::FuncTy(func) => ast::Ty::Func {
            range: func.0.text_range(),
            arg: Rc::new(check_type(env, func.arg())?),
            res: Rc::new(check_type(env, func.res())?),
        },
        TypeKind::ParenTy(ty) => check_type(env, ty.ty()),
        TypeKind::IntTy(int) => ast::Ty::Int {
            range: int.0.text_range(),
        },
        TypeKind::BoolTy(bool) => ast::Ty::Bool {
            range: bool.0.text_range(),
        },
    };
    Ok(ty)
}

fn check(env: Env, expr: cst::Expr, ty: ast::Ty) -> Result<ast::Expr, TypeErr> {
    let expr = infer(env, expr)?;
    let actual_ty = expr.ty();
    if actual_ty.as_ref() == &ty {
        //TODO: type equivalence
        Ok(expr)
    } else {
        Err(TypeMismatch {
            expected: ty,
            actual: actual_ty.as_ref().clone(),
        })
    }
}
