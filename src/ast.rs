use crate::cst;
use crate::cst::ExprKind;
use std::rc::Rc;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Expr {
    Var(String),
    Lambda(String, Rc<Expr>),
    App(Rc<Expr>, Rc<Expr>),
    IntLit(i32),
    BoolLit(bool),
}

impl Expr {
    pub fn from_cst(cst: cst::Expr) -> Option<Self> {
        match cst.kind() {
            ExprKind::Var(var) => Some(Expr::Var(var.name())),
            ExprKind::Lambda(lambda) => {
                let binder = lambda.binder()?;
                let body = lambda.body()?;
                let body = Self::from_cst(body)?;
                Some(Expr::Lambda(binder, Rc::new(body)))
            }
            ExprKind::Application(app) => {
                let func = Self::from_cst(app.func())?;
                let arg = Self::from_cst(app.arg())?;
                Some(Expr::App(Rc::new(func), Rc::new(arg)))
            }
            ExprKind::IntLit(int) => Some(Expr::IntLit(int.value())),
            ExprKind::BooleanLit(bool) => Some(Expr::BoolLit(bool.value())),
        }
    }
}
