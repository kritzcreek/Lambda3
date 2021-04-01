use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

use rowan::TextRange;

use crate::ast;
use crate::ast::{Binding, Name, Ty};
use crate::syntax::ast::AstNode;
use crate::syntax::nodes::{Expr, Pattern};
use crate::syntax::{nodes as cst, LiteralKind};
use crate::types::TypeErr::{NotAFunction, TypeMismatch, UnknownVar};

use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};

#[derive(Debug)]
enum TypeErr {
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

    pub fn extend_with_binding(&self, binding: ast::Binding) -> Env {
        self.extend(binding.ident, binding.ty, binding.range)
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

pub struct Typechecker {
    pub top_level: cst::Expr,
}

impl Typechecker {
    pub fn infer_expr(&self) -> Result<ast::Expr, String> {
        self.infer(&Env::new(), self.top_level.clone())
            .map_err(|err| match err {
                TypeErr::UnknownVar { var, span } => {
                    let err_msg = &format!("unknown var {}", var);
                    let source = &self.top_level.to_string();

                    let snippet = Snippet {
                        title: Some(Annotation {
                            label: Some(err_msg),
                            id: None,
                            annotation_type: AnnotationType::Error,
                        }),
                        footer: vec![],
                        slices: vec![Slice {
                            source,
                            line_start: 1,
                            origin: None,
                            fold: true,
                            annotations: vec![SourceAnnotation {
                                label: err_msg,
                                annotation_type: AnnotationType::Error,
                                range: (span.start().into(), span.end().into()),
                            }],
                        }],
                        opt: FormatOptions {
                            color: true,
                            ..Default::default()
                        },
                    };

                    format!("{}", DisplayList::from(snippet))
                }
                TypeErr::TypeMismatch { expected, actual } => {
                    let source = &self.top_level.to_string();
                    let err_msg = &format!("expected {}, but got {}", expected, actual);
                    let expected_range = expected.text_range();
                    let actual_range = actual.text_range();

                    let snippet = Snippet {
                        title: Some(Annotation {
                            label: Some(err_msg),
                            id: None,
                            annotation_type: AnnotationType::Error,
                        }),
                        footer: vec![],
                        slices: vec![Slice {
                            source,
                            line_start: 1,
                            origin: None,
                            fold: true,
                            annotations: vec![
                                SourceAnnotation {
                                    label: "expected",
                                    annotation_type: AnnotationType::Error,
                                    range: (
                                        expected_range.start().into(),
                                        expected_range.end().into(),
                                    ),
                                },
                                SourceAnnotation {
                                    label: "actual",
                                    annotation_type: AnnotationType::Error,
                                    range: (actual_range.start().into(), actual_range.end().into()),
                                },
                            ],
                        }],
                        opt: FormatOptions {
                            color: true,
                            ..Default::default()
                        },
                    };

                    format!("{}", DisplayList::from(snippet))
                }
                TypeErr::NotAFunction { actual } => {
                    let err_msg = &format!("{} is not a function", actual);
                    let source = &self.top_level.to_string();
                    let range = actual.text_range();

                    let snippet = Snippet {
                        title: Some(Annotation {
                            label: Some(err_msg),
                            id: None,
                            annotation_type: AnnotationType::Error,
                        }),
                        footer: vec![],
                        slices: vec![Slice {
                            source,
                            line_start: 1,
                            origin: None,
                            fold: true,
                            annotations: vec![SourceAnnotation {
                                label: "this expression does not have a function type",
                                annotation_type: AnnotationType::Error,
                                range: (range.start().into(), range.end().into()),
                            }],
                        }],
                        opt: FormatOptions {
                            color: true,
                            ..Default::default()
                        },
                    };

                    format!("{}", DisplayList::from(snippet))
                }
            })
    }

    fn infer(&self, env: &Env, expr: cst::Expr) -> Result<ast::Expr, TypeErr> {
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
                let range = TextRange::new(
                    lambda.backslash_token().unwrap().text_range().start(),
                    lambda.syntax.text_range().end(),
                );
                let binder = lambda.binder().unwrap();
                let body = lambda.body().unwrap();

                let ty_binder = self.check_type(env, binder.ty().unwrap())?;
                let ty_binder = Rc::new(ty_binder);
                let binder_ident = match binder.pattern().unwrap() {
                    Pattern::VarP(v) => v.name(),
                    Pattern::AnnotationP(_) | Pattern::WildcardP(_) | Pattern::ParenP(_) => {
                        panic!("Non-VarP annotation in lambda")
                    }
                };
                let ty_range = binder.ty().unwrap().syntax().text_range();
                let binding = Binding {
                    ident: binder_ident.clone(),
                    ty: ty_binder.clone(),
                    range: ty_range,
                };
                let ty_body =
                    self.infer(&env.extend(binder_ident, ty_binder.clone(), ty_range), body)?;
                let ty_lambda = ast::Ty::Func {
                    arg: ty_binder,
                    res: ty_body.ty(),
                    range,
                };
                Ok(ast::Expr::Lambda {
                    range,
                    ty: Rc::new(ty_lambda),
                    body: Rc::new(ty_body),
                    binder: binding,
                })
            }
            cst::Expr::ApplicationE(app) => {
                let typed_func = self.infer(env, app.func().unwrap())?;
                let (ty_arg, ty_res) = match typed_func.ty().as_ref() {
                    Ty::Func { arg, res, .. } => (arg.clone(), res.clone()),
                    ty => return Err(NotAFunction { actual: ty.clone() }),
                };
                let typed_arg = self.check(
                    env,
                    app.arg().unwrap().expr().unwrap(),
                    ty_arg.as_ref().clone(),
                )?;
                Ok(ast::Expr::App {
                    ty: ty_res,
                    range: app.syntax.text_range(),
                    func: Rc::new(typed_func),
                    arg: Rc::new(typed_arg),
                })
            }

            cst::Expr::LiteralE(lit) => match lit.kind().unwrap() {
                LiteralKind::Int(i, tkn) => {
                    let range = tkn.text_range();
                    Ok(ast::Expr::Lit {
                        range,
                        ty: Rc::new(ast::Ty::Int { range }),
                        lit: ast::Lit::Int(i),
                    })
                }
                LiteralKind::Bool(b, tkn) => {
                    let range = tkn.text_range();
                    Ok(ast::Expr::Lit {
                        range,
                        ty: Rc::new(ast::Ty::Bool { range }),
                        lit: ast::Lit::Bool(b),
                    })
                }
            },
            Expr::ParenE(paren_e) => {
                let inner = paren_e.expr().unwrap();
                self.infer(env, inner)
            }
            Expr::LetE(let_e) => {
                let pattern = let_e.pattern().unwrap();
                let expr = let_e.expr().unwrap();
                let body = let_e.body().unwrap().expr().unwrap();

                let typed_expr = self.infer(env, expr)?;
                let binding = self.check_pattern(env, pattern, typed_expr.ty())?;
                let temp_env = env.extend_with_binding(binding.clone());

                let typed_body = self.infer(&temp_env, body)?;
                Ok(ast::Expr::Let {
                    ty: typed_body.ty(),
                    range: let_e.syntax.text_range(),
                    binder: binding,
                    expr: Rc::new(typed_expr),
                    body: Rc::new(typed_body),
                })
            }
        }
    }

    //TODO should return ast::Pattern
    fn check_pattern(
        &self,
        _env: &Env,
        pattern: cst::Pattern,
        ty: Rc<ast::Ty>,
    ) -> Result<ast::Binding, TypeErr> {
        match pattern {
            Pattern::VarP(ref v) => Ok(Binding {
                ident: v.name(),
                range: pattern.syntax().text_range(),
                ty,
            }),
            Pattern::AnnotationP(_) | Pattern::WildcardP(_) | Pattern::ParenP(_) => {
                panic!("Non-VarP annotation in check pattern")
            }
        }
    }

    fn check_type(&self, env: &Env, ty: cst::Type) -> Result<ast::Ty, TypeErr> {
        let ty = match ty {
            cst::Type::FuncTy(func) => ast::Ty::Func {
                // TODO: Do proper ranges
                range: func.syntax.text_range(),
                arg: Rc::new(self.check_type(env, func.arg().unwrap().ty().unwrap())?),
                res: Rc::new(self.check_type(env, func.result().unwrap().ty().unwrap())?),
            },
            cst::Type::ParenTy(ty) => self.check_type(env, ty.ty().unwrap())?,
            cst::Type::IntTy(int) => ast::Ty::Int {
                range: int.Int_token().unwrap().text_range(),
            },
            cst::Type::BoolTy(bool) => ast::Ty::Bool {
                range: bool.Bool_token().unwrap().text_range(),
            },
        };
        Ok(ty)
    }

    fn check(&self, env: &Env, expr: cst::Expr, ty: ast::Ty) -> Result<ast::Expr, TypeErr> {
        let expr = self.infer(env, expr)?;
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
}
