use crate::cst;
use crate::cst::ExprKind;
use rowan::TextRange;
use std::rc::Rc;

#[derive(Clone, Eq, PartialEq, Debug)]
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
        result: Rc<Ty>,
    },
    Forall {
        range: TextRange,
        binding: Binding,
        ty: Rc<Ty>,
    },
    Var {
        range: TextRange,
        ident: String,
    },
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Binding {
    range: TextRange,
    ty: Rc<Ty>,
    ident: String,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Expr {
    Var {
        ty: Rc<Ty>,
        range: TextRange,
        ident: String,
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
        binder: (TextRange, String),
        body: Rc<Expr>,
    },
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Lit {
    Int(i32),
    Bool(bool),
}

fn var(range: TextRange, ident: String) -> Expr {
    Expr::Var {
        ty: Rc::new(Ty::Int { range }),
        range,
        ident,
    }
}

impl Expr {
    // pub fn from_cst(cst: cst::Expr) -> Option<Self> {
    //     match cst.kind() {
    //         ExprKind::Var(v) => Some(var(v.0.text_range(), v.name())),
    //         ExprKind::Lambda(lambda) => {
    //             let binder = lambda.binder()?;
    //             let body = lambda.body()?;
    //             let body = Self::from_cst(body)?;
    //             Some(Expr::Lambda(binder, Rc::new(body)))
    //         }
    //         ExprKind::Application(app) => {
    //             let func = Self::from_cst(app.func())?;
    //             let arg = Self::from_cst(app.arg())?;
    //             Some(Expr::App(Rc::new(func), Rc::new(arg)))
    //         }
    //         ExprKind::IntLit(int) => Some(Expr::IntLit(int.value())),
    //         ExprKind::BooleanLit(bool) => Some(Expr::BoolLit(bool.value())),
    //     }
    // }

    pub fn ty(&self) -> Rc<Ty> {
        match self {
            Expr::Var { ty, .. } => ty.clone(),
            Expr::Lambda  { ty, .. } => ty.clone(),
            Expr::App  { ty, .. } => ty.clone(),
            Expr::Lit  { ty, .. } => ty.clone(),
            Expr::TyApp  { ty, .. } => ty.clone(),
            Expr::TyLambda  { ty, .. } => ty.clone(),
        }
    }
}
