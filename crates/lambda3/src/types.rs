use std::collections::HashMap;
use std::rc::Rc;

use rowan::TextRange;

use crate::ast;
use crate::ast::{Binding, Name, Ty};
use crate::syntax::nodes as cst;
use crate::syntax::nodes::Expr;
use crate::types::TypeErr::{NotAFunction, TypeMismatch, UnknownVar};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hint::unreachable_unchecked;

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
    match expr {
        cst::Expr::VarE(var) => {
            let ident_token = var.ident_token().unwrap();
            let range = ident_token.text_range();
            let ident = ident_token.text().to_string();
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
        cst::Expr::LambdaE(lambda) => {
            // let range = lambda.0.text_range();
            // let binder = lambda.binder().unwrap();
            // let body = lambda.body().unwrap();
            //
            // let ty_binder = check_type(env, binder.ty())?;
            // let ty_binder = Rc::new(ty_binder);
            // let binder_ident = match binder.pattern().kind() {
            //     PatternKind::VarP(v) => v.name(),
            //     PatternKind::ParenP(_)
            //     | PatternKind::WildcardP(_)
            //     | PatternKind::AnnotationP(_) => panic!(),
            // };
            // let binding = Binding {
            //     ident: binder_ident.clone(),
            //     ty: ty_binder.clone(),
            //     range: binder.ty().range(),
            // };
            // let ty_body = infer(
            //     &env.extend(binder_ident, ty_binder.clone(), binder.ty().range()),
            //     body,
            // )?;
            // let ty_lambda = ast::Ty::Func {
            //     arg: ty_binder,
            //     res: ty_body.ty(),
            //     range,
            // };
            // Ok(ast::Expr::Lambda {
            //     range,
            //     ty: Rc::new(ty_lambda),
            //     body: Rc::new(ty_body),
            //     binder: binding,
            // })
            unreachable!()
        }
        cst::Expr::ApplicationE(app) => {
            let typed_func = infer(env, app.func().unwrap())?;
            let (ty_arg, ty_res) = match typed_func.ty().as_ref() {
                Ty::Func { arg, res, .. } => (arg.clone(), res.clone()),
                ty => return Err(NotAFunction { actual: ty.clone() }),
            };
            let typed_arg = check(env, app.arg().unwrap(), ty_arg.as_ref().clone())?;
            Ok(ast::Expr::App {
                ty: ty_res,
                range: app.syntax.text_range(),
                func: Rc::new(typed_func),
                arg: Rc::new(typed_arg),
            })
        }

        cst::Expr::LiteralE(lit) => {
            unreachable!()
            // let range = lit.0.text_range();
            // Ok(ast::Expr::Lit {
            //     range,
            //     ty: Rc::new(ast::Ty::Bool { range }),
            //     lit: ast::Lit::Bool(lit.bool()),
            // })
            //
            // cst::Expr::IntLit(lit) => {
            //     let range = lit.0.text_range();
            //     Ok(ast::Expr::Lit {
            //         range,
            //         ty: Rc::new(ast::Ty::Int { range }),
            //         lit: ast::Lit::Int(lit.int()),
            //     })
            // }
        }
        Expr::ParenE(paren_e) => {
            unreachable!()
        }
    }
}

fn check_type(env: &Env, ty: cst::Type) -> Result<ast::Ty, TypeErr> {
    let ty = match ty {
        cst::Type::FuncTy(func) => ast::Ty::Func {
            // TODO: Do proper ranges
            range: func.syntax.text_range(),
            arg: Rc::new(check_type(env, func.arg().unwrap().ty().unwrap())?),
            res: Rc::new(check_type(env, func.result().unwrap().ty().unwrap())?),
        },
        cst::Type::ParenTy(ty) => check_type(env, ty.ty().unwrap())?,
        cst::Type::IntTy(int) => ast::Ty::Int {
            range: int.Int_token().unwrap().text_range(),
        },
        cst::Type::BoolTy(bool) => ast::Ty::Bool {
            range: bool.Bool_token().unwrap().text_range(),
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
