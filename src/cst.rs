use num_traits::{FromPrimitive, ToPrimitive};
/// You can construct GreenNodes by hand, but a builder
/// is helpful for top-down parsers: it maintains a stack
/// of currently in-progress nodes
use rowan::GreenNodeBuilder;
use rowan::SmolStr;
/// GreenNode is an immutable tree, which is cheap to change,
/// but doesn't contain offsets and parent pointers.
use rowan::{Checkpoint, GreenNode};

use crate::lexer::{
    lex_str,
    SyntaxKind::{self, *},
};
use crate::parser::{self, Parse, Parser};
use crate::syntax::{Lang, SyntaxNode, SyntaxToken};

pub fn parse(text: &str) -> Parse {
    parser::parse(text)
}

macro_rules! ast_node {
    ($ast:ident, $kind:ident) => {
        #[derive(PartialEq, Eq, Hash, Debug)]
        #[repr(transparent)]
        pub struct $ast(pub SyntaxNode);
        impl $ast {
            #[allow(unused)]
            pub fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == $kind {
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
                if node.kind() == LiteralE && first_token(&node)?.kind() == $kind {
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
                if node.kind() == LiteralE {
                    let token = first_token(&node)?.kind();
                    if token == $kind1 || token == $kind2 {
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

ast_node!(Root, RootE);
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

impl Root {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

pub fn first_token(node: &SyntaxNode) -> Option<SyntaxToken> {
    node.children_with_tokens().find_map(|node| {
        if node.kind() != Whitespace {
            node.as_token().cloned()
        } else {
            None
        }
    })
}

pub fn first_word(node: &SyntaxNode) -> Option<String> {
    node.children_with_tokens().find_map(|node| {
        if node.kind() == Ident {
            Some(node.as_token().unwrap().text().to_string())
        } else {
            None
        }
    })
}

impl Lambda {
    pub fn binder(&self) -> Option<String> {
        self.0
            .children()
            .find(|node| node.kind() == VarP)
            .and_then(|node| first_word(&node))
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
        first_token(&self.0).unwrap().kind() == TrueKw
    }
}

impl Application {
    pub fn func(&self) -> Expr {
        self.0.children().filter_map(Expr::cast).nth(0).unwrap()
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
    fn test() {
        use std::fs;

        glob!("examples/expr/*.l3", |path| {
            let input = fs::read_to_string(path).unwrap();
            let parse = parse(&input);
            let node = parse.syntax();

            assert_debug_snapshot!((input, parse.errors, node));
        });
    }
}
