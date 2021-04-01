use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

use rowan::TextRange;

#[derive(Clone, Debug)]
pub enum Ty {
    Int {
        range: TextRange,
    },
    Bool {
        range: TextRange,
    },
    Func {
        range: TextRange,
        arg: Rc<Ty>,
        res: Rc<Ty>,
    },
    Forall {
        range: TextRange,
        var: Name,
        ty: Rc<Ty>,
    },
    Var {
        range: TextRange,
        name: Name,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)] // TODO Ignore range for Eq
pub struct Name {
    pub range: TextRange,
    pub ident: String,
}

impl Eq for Ty {}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Ty::Int { .. }, Ty::Int { .. }) => true,
            (Ty::Bool { .. }, Ty::Bool { .. }) => true,
            (
                Ty::Func {
                    arg: arg1,
                    res: res1,
                    ..
                },
                Ty::Func {
                    arg: arg2,
                    res: res2,
                    ..
                },
            ) => arg1.as_ref() == arg2.as_ref() && res1.as_ref() == res2.as_ref(),
            (
                Ty::Forall {
                    var: var1, ty: ty1, ..
                },
                Ty::Forall {
                    var: var2, ty: ty2, ..
                },
            ) =>
            //TODO: alpha eq?
            {
                var1.ident == var2.ident && ty1.as_ref() == ty2.as_ref()
            }
            (Ty::Var { name: name1, .. }, Ty::Var { name: name2, .. }) => {
                name1.ident == name2.ident
            }
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub range: TextRange,
    pub ty: Rc<Ty>,
    pub ident: String,
}

impl Eq for Binding {}

impl PartialEq for Binding {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.ty.as_ref() == other.ty.as_ref()
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Expr {
    Var {
        ty: Rc<Ty>,
        name: Name,
    },
    Lambda {
        ty: Rc<Ty>,
        range: TextRange,
        binder: Binding,
        body: Rc<Expr>,
    },
    App {
        ty: Rc<Ty>,
        range: TextRange,
        func: Rc<Expr>,
        arg: Rc<Expr>,
    },
    Lit {
        ty: Rc<Ty>,
        range: TextRange,
        lit: Lit,
    },
    TyApp {
        ty: Rc<Ty>,
        range: TextRange,
        func: Rc<Expr>,
        arg: Rc<Ty>,
    },

    TyLambda {
        ty: Rc<Ty>,
        range: TextRange,
        binder: Name,
        body: Rc<Expr>,
    },
    Let {
        ty: Rc<Ty>,
        range: TextRange,
        binder: Binding,
        expr: Rc<Expr>,
        body: Rc<Expr>,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Lit { lit, .. } => {
                write!(f, "{}", lit)
            }
            Expr::Var { name, .. } => {
                write!(f, "{}", name.ident)
            }
            Expr::Lambda { binder, body, .. } => {
                write!(f, "(\\{}. {})", binder, body)
            }
            Expr::App { func, arg, .. } => {
                write!(f, "({} {})", func, arg)
            }
            Expr::TyApp { func, arg, .. } => {
                write!(f, "({} {})", func, arg)
            }
            Expr::TyLambda { binder, body, .. } => {
                write!(f, "(Λ{}. {})", binder.ident, body)
            }
            Expr::Let {
                binder, expr, body, ..
            } => {
                write!(f, "(let {} = {} in {})", binder.ident, expr, body)
            }
        }
    }
}

impl Display for Lit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Int(int) => write!(f, "{}", int),
            Lit::Bool(bool) => write!(f, "{}", bool),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Int { .. } => {
                write!(f, "Int")
            }
            Ty::Bool { .. } => {
                write!(f, "Bool")
            }
            Ty::Var { name, .. } => {
                write!(f, "{}", name.ident)
            }
            Ty::Forall { var, ty, .. } => {
                write!(f, "forall {}. {}", var.ident, ty)
            }
            Ty::Func { arg, res, .. } => {
                write!(f, "({} -> {})", arg, res)
            }
        }
    }
}

impl Display for Binding {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.ident, self.ty)
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Lit {
    Int(i32),
    Bool(bool),
}

pub fn var(range: TextRange, ident: String) -> Expr {
    Expr::Var {
        ty: Rc::new(Ty::Int { range }),
        name: Name { ident, range },
    }
}

impl Expr {
    pub fn ty(&self) -> Rc<Ty> {
        match self {
            Expr::Var { ty, .. } => ty.clone(),
            Expr::Lambda { ty, .. } => ty.clone(),
            Expr::App { ty, .. } => ty.clone(),
            Expr::Lit { ty, .. } => ty.clone(),
            Expr::TyApp { ty, .. } => ty.clone(),
            Expr::TyLambda { ty, .. } => ty.clone(),
            Expr::Let { ty, .. } => ty.clone(),
        }
    }
}

impl Ty {
    pub fn text_range(&self) -> &TextRange {
        match self {
            Ty::Int { range, .. } => range,
            Ty::Bool { range, .. } => range,
            Ty::Func { range, .. } => range,
            Ty::Forall { range, .. } => range,
            Ty::Var { range, .. } => range,
        }
    }
}
