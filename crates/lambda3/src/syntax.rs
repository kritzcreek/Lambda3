use crate::lexer::SyntaxKind;
use num_traits::{FromPrimitive, ToPrimitive};
pub(crate) mod ast;
#[allow(unused)]
#[allow(non_snake_case)]
#[allow(clippy::all)]
pub mod nodes;

/// Second, implementing the `Language` trait teaches rowan to convert between
/// these two SyntaxKind types, allowing for a nicer SyntaxNode API where
/// "kinds" are values from our `enum SyntaxKind`, instead of plain u16 values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}

pub type SyntaxNode = rowan::SyntaxNode<Lang>;
pub type SyntaxToken = rowan::SyntaxToken<Lang>;
#[allow(unused)]
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<Lang>;
#[allow(unused)]
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<Lang>;

impl rowan::Language for Lang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        Self::Kind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.to_u16().unwrap())
    }
}

// Manually implemented accessors
pub enum LiteralKind {
    Int(i32, SyntaxToken),
    Bool(bool, SyntaxToken),
}

impl nodes::LiteralE {
    pub fn kind(&self) -> Option<LiteralKind> {
        if let Some(num_token) = self.number_lit_token() {
            Some(LiteralKind::Int(
                str::parse::<i32>(num_token.text()).unwrap(),
                num_token,
            ))
        } else if let Some(true_token) = self.true_token() {
            Some(LiteralKind::Bool(true, true_token))
        } else if let Some(false_token) = self.false_token() {
            Some(LiteralKind::Bool(false, false_token))
        } else {
            None
        }
    }
}

impl nodes::VarP {
    pub fn name(&self) -> String {
        self.ident_token().unwrap().text().to_string()
    }
}

impl nodes::VarE {
    pub fn name(&self) -> String {
        self.ident_token().unwrap().text().to_string()
    }
}
