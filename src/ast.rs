use rowan::TextRange;
use std::rc::Rc;

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
        binding: Binding,
        ty: Rc<Ty>,
    },
    Var {
        range: TextRange,
        ident: String,
    },
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
                    binding: binding1,
                    ty: ty1,
                    ..
                },
                Ty::Forall {
                    binding: binding2,
                    ty: ty2,
                    ..
                },
            ) =>
            //TODO: alpha eq?
            {
                binding1 == binding2 && ty1.as_ref() == ty2.as_ref()
            }
            (Ty::Var { ident: ident1, .. }, Ty::Var { ident: ident2, .. }) => ident1 == ident2,
            _ => false,
        }
    }

    fn ne(&self, other: &Self) -> bool {
        !(self == other)
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

    fn ne(&self, other: &Self) -> bool {
        !(self == other)
    }
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

pub fn var(range: TextRange, ident: String) -> Expr {
    Expr::Var {
        ty: Rc::new(Ty::Int { range }),
        range,
        ident,
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
        }
    }
}
