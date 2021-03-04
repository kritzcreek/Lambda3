use std::collections::HashMap;
use std::rc::Rc;

use rowan::TextRange;

use crate::ast;
use crate::ast::{Binding, Name, Ty};
use crate::cst::{self, ExprKind, PatternKind, TypeKind};
use crate::types::TypeErr::{NotAFunction, TypeMismatch, UnknownVar};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug)]
pub enum TypeErr {
    UnknownVar { var: String, span: TextRange },
    TypeMismatch { expected: ast::Ty, actual: ast::Ty },
    NotAFunction { actual: ast::Ty },
}

struct Env {
    values: HashMap<String, (Rc<ast::Ty>, TextRange)>,
    types: HashMap<String, TextRange>,
}

impl Env {
    pub fn extend(&self, name: String, ty: Rc<ast::Ty>, range: TextRange) -> Env {
        let mut vals = self.values.clone();
        vals.insert(name, (ty, range));
        Env {
            values: vals,
            types: self.types.clone(),
        }
    }

    pub fn new() -> Env {
        Env {
            values: HashMap::new(),
            types: HashMap::new(),
        }
    }
}

impl Display for TypeErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeErr::UnknownVar { var, span } => {
                write!(f, "unknown var {} at {:?}", var, span)
            }
            TypeErr::TypeMismatch { expected, actual } => {
                write!(f, "expected {}, but got {}", expected, actual)
            }
            TypeErr::NotAFunction { actual } => {
                write!(f, "{} is not a function", actual)
            }
        }
    }
}

pub fn infer_expr(expr: cst::Expr) -> Result<ast::Expr, TypeErr> {
    infer(&Env::new(), expr)
}

fn infer(env: &Env, expr: cst::Expr) -> Result<ast::Expr, TypeErr> {
    match expr.kind() {
        ExprKind::Var(var) => {
            let range = var.0.text_range();
            let ident = var.name();
            match env.values.get(&ident) {
                None => Err(UnknownVar {
                    var: ident,
                    span: range,
                }),
                Some((ty, _)) => Ok(ast::Expr::Var {
                    name: Name { ident, range },
                    ty: Rc::clone(ty),
                }),
            }
        }
        ExprKind::Lambda(lambda) => {
            let range = lambda.0.text_range();
            let binder = lambda.binder().unwrap();
            let body = lambda.body().unwrap();

            let ty_binder = check_type(env, binder.ty())?;
            let ty_binder = Rc::new(ty_binder);
            let binder_ident = match binder.pattern().kind() {
                PatternKind::VarP(v) => v.name(),
                PatternKind::ParenP(_)
                | PatternKind::WildcardP(_)
                | PatternKind::AnnotationP(_) => panic!(),
            };
            let binding = Binding {
                ident: binder_ident.clone(),
                ty: ty_binder.clone(),
                range: binder.ty().range(),
            };
            let ty_body = infer(
                &env.extend(binder_ident, ty_binder.clone(), binder.ty().range()),
                body,
            )?;
            let ty_lambda = ast::Ty::Func {
                arg: ty_binder,
                res: ty_body.ty().clone(),
                range,
            };
            Ok(ast::Expr::Lambda {
                range,
                ty: Rc::new(ty_lambda),
                body: Rc::new(ty_body),
                binder: binding,
            })
        }
        ExprKind::Application(app) => {
            let typed_func = infer(env, app.func())?;
            let (ty_arg, ty_res) = match typed_func.ty().as_ref() {
                Ty::Func { arg, res, .. } => (arg.clone(), res.clone()),
                ty => return Err(NotAFunction { actual: ty.clone() }),
            };
            let typed_arg = check(env, app.arg(), ty_arg.as_ref().clone())?;
            Ok(ast::Expr::App {
                ty: ty_res,
                range: app.0.text_range(),
                func: Rc::new(typed_func),
                arg: Rc::new(typed_arg),
            })
        }

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
        TypeKind::ParenTy(ty) => check_type(env, ty.ty())?,
        TypeKind::IntTy(int) => ast::Ty::Int {
            range: int.0.text_range(),
        },
        TypeKind::BoolTy(bool) => ast::Ty::Bool {
            range: bool.0.text_range(),
        },
    };
    Ok(ty)
}

fn check(env: &Env, expr: cst::Expr, ty: ast::Ty) -> Result<ast::Expr, TypeErr> {
    let expr = infer(env, expr)?;
    let actual_ty = expr.ty();
    if actual_ty.as_ref() == &ty {
        Ok(expr)
    } else {
        Err(TypeMismatch {
            expected: ty,
            actual: actual_ty.as_ref().clone(),
        })
    }
}
