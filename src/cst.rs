use crate::lexer::SyntaxKind;
use crate::parser::{self, Parse};
use crate::syntax::{SyntaxNode, SyntaxToken};
use rowan::TextRange;

pub fn parse(text: &str) -> Parse {
    parser::parse(text)
}

pub fn parse_type(text: &str) -> Parse {
    parser::parse_type(text)
}

pub fn parse_pattern(text: &str) -> Parse {
    parser::parse_pattern(text)
}

macro_rules! ast_node {
    ($ast:ident, $kind:ident) => {
        #[derive(PartialEq, Eq, Hash, Debug)]
        #[repr(transparent)]
        pub struct $ast(pub SyntaxNode);
        impl $ast {
            #[allow(unused)]
            pub fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == SyntaxKind::$kind {
                    Some(Self(node))
                } else {
                    None
                }
            }
        }
    };
}

macro_rules! lit_node {
    ($ast:ident, $kind:ident) => {
        #[derive(PartialEq, Eq, Hash, Debug)]
        #[repr(transparent)]
        pub struct $ast(pub SyntaxNode);
        impl $ast {
            #[allow(unused)]
            fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == SyntaxKind::LiteralE
                    && first_token(&node)?.kind() == SyntaxKind::$kind
                {
                    Some(Self(node))
                } else {
                    None
                }
            }
        }
    };
    ($ast:ident, $kind1:ident, $kind2:ident) => {
        #[derive(PartialEq, Eq, Hash, Debug)]
        #[repr(transparent)]
        pub struct $ast(pub SyntaxNode);
        impl $ast {
            #[allow(unused)]
            fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == SyntaxKind::LiteralE {
                    let token = first_token(&node)?.kind();
                    if token == SyntaxKind::$kind1 || token == SyntaxKind::$kind2 {
                        Some(Self(node))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    };
}

ast_node!(Root, Root);
ast_node!(Var, VarE);
ast_node!(Lambda, LambdaE);
ast_node!(Application, ApplicationE);
lit_node!(IntLit, NumberLit);
lit_node!(BooleanLit, TrueKw, FalseKw);

#[derive(PartialEq, Eq, Hash, Debug)]
#[repr(transparent)]
pub struct Expr(SyntaxNode);

#[derive(Debug)]
pub enum ExprKind {
    Var(Var),
    Lambda(Lambda),
    Application(Application),
    IntLit(IntLit),
    BooleanLit(BooleanLit),
}

ast_node!(VarP, VarP);
ast_node!(ParenP, ParenP);
ast_node!(WildcardP, WildcardP);
ast_node!(AnnotationP, AnnotationP);

#[derive(PartialEq, Eq, Hash, Debug)]
#[repr(transparent)]
pub struct Pattern(SyntaxNode);

#[derive(Debug)]
pub enum PatternKind {
    VarP(VarP),
    ParenP(ParenP),
    WildcardP(WildcardP),
    AnnotationP(AnnotationP),
}

ast_node!(ParenTy, ParenTy);
ast_node!(IntTy, IntTy);
ast_node!(BoolTy, BoolTy);
ast_node!(FuncTy, FuncTy);

#[derive(PartialEq, Eq, Hash, Debug)]
#[repr(transparent)]
pub struct Type(SyntaxNode);

#[derive(Debug)]
pub enum TypeKind {
    ParenTy(ParenTy),
    IntTy(IntTy),
    BoolTy(BoolTy),
    FuncTy(FuncTy),
}

impl Type {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if ParenTy::cast(node.clone()).is_some()
            || IntTy::cast(node.clone()).is_some()
            || BoolTy::cast(node.clone()).is_some()
            || FuncTy::cast(node.clone()).is_some()
        {
            Some(Type(node))
        } else {
            None
        }
    }

    pub fn kind(&self) -> TypeKind {
        ParenTy::cast(self.0.clone())
            .map(TypeKind::ParenTy)
            .or_else(|| IntTy::cast(self.0.clone()).map(TypeKind::IntTy))
            .or_else(|| BoolTy::cast(self.0.clone()).map(TypeKind::BoolTy))
            .or_else(|| FuncTy::cast(self.0.clone()).map(TypeKind::FuncTy))
            .unwrap()
    }

    pub fn range(&self) -> TextRange {
        self.0.text_range()
    }
}

impl Expr {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if Var::cast(node.clone()).is_some()
            || Lambda::cast(node.clone()).is_some()
            || Application::cast(node.clone()).is_some()
            || IntLit::cast(node.clone()).is_some()
            || BooleanLit::cast(node.clone()).is_some()
        {
            Some(Expr(node))
        } else {
            None
        }
    }

    pub fn kind(&self) -> ExprKind {
        Var::cast(self.0.clone())
            .map(ExprKind::Var)
            .or_else(|| Lambda::cast(self.0.clone()).map(ExprKind::Lambda))
            .or_else(|| Application::cast(self.0.clone()).map(ExprKind::Application))
            .or_else(|| IntLit::cast(self.0.clone()).map(ExprKind::IntLit))
            .or_else(|| BooleanLit::cast(self.0.clone()).map(ExprKind::BooleanLit))
            .unwrap()
    }
}

impl Pattern {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if VarP::cast(node.clone()).is_some()
            || ParenP::cast(node.clone()).is_some()
            || WildcardP::cast(node.clone()).is_some()
            || AnnotationP::cast(node.clone()).is_some()
        {
            Some(Pattern(node))
        } else {
            None
        }
    }

    pub fn kind(&self) -> PatternKind {
        VarP::cast(self.0.clone())
            .map(PatternKind::VarP)
            .or_else(|| ParenP::cast(self.0.clone()).map(PatternKind::ParenP))
            .or_else(|| WildcardP::cast(self.0.clone()).map(PatternKind::WildcardP))
            .or_else(|| AnnotationP::cast(self.0.clone()).map(PatternKind::AnnotationP))
            .unwrap()
    }
}
impl ParenTy {
    pub fn ty(&self) -> Type {
        self.0.children().find_map(Type::cast).unwrap()
    }
}

impl FuncTy {
    pub fn arg(&self) -> Type {
        self.0
            .children()
            .find_map(|x| {
                if x.kind() == SyntaxKind::TyArg {
                    x.children().find_map(Type::cast)
                } else {
                    None
                }
            })
            .unwrap()
    }

    pub fn res(&self) -> Type {
        self.0
            .children()
            .find_map(|x| {
                if x.kind() == SyntaxKind::TyRes {
                    x.children().find_map(Type::cast)
                } else {
                    None
                }
            })
            .unwrap()
    }
}

impl ParenP {
    pub fn pattern(&self) -> Pattern {
        self.0.children().find_map(Pattern::cast).unwrap()
    }
}
impl AnnotationP {
    pub fn pattern(&self) -> Pattern {
        self.0.children().find_map(Pattern::cast).unwrap()
    }

    pub fn ty(&self) -> Type {
        self.0.children().find_map(Type::cast).unwrap()
    }
}

impl IntLit {
    pub fn int(&self) -> i32 {
        self.0.text().to_string().parse().unwrap()
    }
}

impl BooleanLit {
    pub fn bool(&self) -> bool {
        self.0.text().to_string().parse().unwrap()
    }
}

impl Root {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

pub fn first_token(node: &SyntaxNode) -> Option<SyntaxToken> {
    node.children_with_tokens().find_map(|node| {
        if node.kind() != SyntaxKind::Whitespace {
            node.as_token().cloned()
        } else {
            None
        }
    })
}

pub fn first_word(node: &SyntaxNode) -> Option<String> {
    node.children_with_tokens().find_map(|node| {
        if node.kind() == SyntaxKind::Ident {
            Some(node.as_token().unwrap().text().to_string())
        } else {
            None
        }
    })
}

impl Lambda {
    pub fn binder(&self) -> Option<AnnotationP> {
        self.0.children().find_map(AnnotationP::cast)
    }

    pub fn body(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl Var {
    pub fn name(&self) -> String {
        first_word(&self.0).unwrap()
    }
}

impl IntLit {
    pub fn value(&self) -> i32 {
        first_token(&self.0).unwrap().text().parse().unwrap()
    }
}

impl BooleanLit {
    pub fn value(&self) -> bool {
        first_token(&self.0).unwrap().kind() == SyntaxKind::TrueKw
    }
}

impl Application {
    pub fn func(&self) -> Expr {
        self.0.children().filter_map(Expr::cast).next().unwrap()
    }

    pub fn arg(&self) -> Expr {
        self.0.children().filter_map(Expr::cast).nth(1).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use insta::{assert_debug_snapshot, glob};

    use super::*;

    #[test]
    fn parse_expr_test() {
        use std::fs;

        glob!("examples/expr/*.l3", |path| {
            let input = fs::read_to_string(path).unwrap();
            let parse = parse(&input);
            let node = parse.syntax();

            assert_debug_snapshot!((input, parse.errors, node));
        });
    }

    #[test]
    fn parse_type_test() {
        use std::fs;

        glob!("examples/types/*.l3", |path| {
            let input = fs::read_to_string(path).unwrap();
            let parse = parse_type(&input);
            let node = parse.syntax();

            assert_debug_snapshot!((input, parse.errors, node));
        });
    }

    #[test]
    fn parse_patterns_test() {
        use std::fs;

        glob!("examples/patterns/*.l3", |path| {
            let input = fs::read_to_string(path).unwrap();
            let parse = parse_pattern(&input);
            let node = parse.syntax();

            assert_debug_snapshot!((input, parse.errors, node));
        });
    }
}
