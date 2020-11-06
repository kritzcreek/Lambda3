use rowan::SmolStr;

// - Lossless
// - Incremental
// - Error-Recovery
// - Parent-Pointer
// - Syntax-Highlighting

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum SyntaxKind {
    L_PAREN = 0, // '('
    R_PAREN,     // ')'
    LAM,         // '\'
    ARROW,       // '->'
    WORD,        // 'x'
    WHITESPACE,  // whitespaces is explicit
    ERROR,       // as well as errors
    EOF,         // end of file

    // composite nodes
    PARENTHESIZED, // `(+ 2 3)`
    VAR,           // `+`, `15`, wraps a WORD token
    LAMBDA,        // a Lambda abstraction
    APPLICATION,   // a function application
    ROOT,          // The top-level node
}

use SyntaxKind::*;

/// Some boilerplate is needed, as rowan settled on using its own
/// `struct SyntaxKind(u16)` internally, instead of accepting the
/// user's `enum SyntaxKind` as a type parameter.
///
/// First, to easily pass the enum variants into rowan via `.into()`:
impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

/// Second, implementing the `Language` trait teaches rowan to convert between
/// these two SyntaxKind types, allowing for a nicer SyntaxNode API where
/// "kinds" are values from our `enum SyntaxKind`, instead of plain u16 values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}

impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= ROOT as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

/// GreenNode is an immutable tree, which is cheap to change,
/// but doesn't contain offsets and parent pointers.
use rowan::GreenNode;

/// You can construct GreenNodes by hand, but a builder
/// is helpful for top-down parsers: it maintains a stack
/// of currently in-progress nodes
use rowan::GreenNodeBuilder;

/// The parse results are stored as a "green tree".
/// We'll discuss working with the results later
pub struct Parse {
    pub green_node: GreenNode,
    #[allow(unused)]
    pub errors: Vec<String>,
}

struct Parser {
    /// input tokens, including whitespace,
    /// in *reverse* order.
    tokens: Vec<(SyntaxKind, SmolStr)>,
    /// the in-progress tree.
    builder: GreenNodeBuilder<'static>,
    /// the list of syntax errors we've accumulated
    /// so far.
    errors: Vec<String>,
}

/// The outcome of parsing a single S-expression
enum ExprRes {
    /// An expression was successfully parsed
    Ok,
    /// An unexpected token was found
    Lul(String),
}

impl Parser {
    fn parse(mut self) -> Parse {
        // Make sure that the root node covers all source
        self.builder.start_node(ROOT.into());
        // Parse zero or more S-expressions
        let _ = self.expr();
        // Don't forget to eat *trailing* whitespace
        self.skip_ws();
        if self.current() != SyntaxKind::EOF {
            self.builder.start_node(ERROR.into());
            while self.current() != EOF {
                self.bump_any()
            }
            self.builder.finish_node();
            self.errors
                .push(format!("Unexpected token {:?}", self.current()))
        }
        // Close the root node.
        self.builder.finish_node();

        // Turn the builder into a GreenNode
        Parse {
            green_node: self.builder.finish(),
            errors: self.errors,
        }
    }
    fn expr(&mut self) -> ExprRes {
        let mut is_application = false;
        let checkpoint = self.builder.checkpoint();
        match self.atom() {
            None => return ExprRes::Lul("Expected expression".to_string()),
            Some(ExprRes::Ok) => (),
            Some(ExprRes::Lul(err)) => {
                self.errors.push(err.clone());
                return ExprRes::Lul(err);
            }
        }
        loop {
            match self.atom() {
                None => break,
                Some(ExprRes::Lul(s)) => {
                    self.report_error(s);
                }
                Some(ExprRes::Ok) => {
                    self.builder.start_node_at(checkpoint, APPLICATION.into());
                    is_application = true;
                }
            }
        }
        if is_application {
            self.builder.finish_node();
        }
        ExprRes::Ok
    }

    // Option<ExprRes>
    // Some(OK) => atom, continue
    // Some(Err) => handle recovery
    // None => no atom, no tokens

    fn atom(&mut self) -> Option<ExprRes> {
        // Either a list, an atom, a closing paren,
        // or an eof.
        match self.current() {
            L_PAREN => self.parse_parenthesized(),
            WORD => self.parse_var(),
            LAM => return self.parse_lambda(),
            ERROR => self.bump_any(),
            _ => return None,
        }

        Some(ExprRes::Ok)
    }

    fn parse_parenthesized(&mut self) {
        self.builder.start_node(PARENTHESIZED.into());
        self.bump(L_PAREN);
        self.expr();
        if !self.eat(R_PAREN) {
            self.builder.start_node(ERROR.into());
            self.errors.push(format!(
                "unexpected token {:?}, expected ')'",
                self.current()
            ));
            while self.current() != R_PAREN && self.current() != EOF {
                self.bump_any()
            }
            self.builder.finish_node();
            self.eat(R_PAREN);
        }
        self.builder.finish_node();
    }

    fn parse_var(&mut self) {
        self.builder.start_node(VAR.into());
        self.bump(WORD);
        self.builder.finish_node();
    }

    fn parse_lambda(&mut self) -> Option<ExprRes> {
        self.builder.start_node(LAMBDA.into());
        self.bump(LAM);
        if !self.eat(WORD) {
            self.report_error(format!("expected binder, got {:?}", self.current()))
        }
        if !self.eat(ARROW) {
            self.builder.finish_node();
            return Some(ExprRes::Lul(format!(
                "expected '->', got {:?}",
                self.current()
            )));
        }

        match self.expr() {
            ExprRes::Ok => self.builder.finish_node(),
            ExprRes::Lul(err) => {
                self.report_error(err);
                self.builder.finish_node();
                return None;
            }
        }

        Some(ExprRes::Ok)
    }

    fn report_error(&mut self, msg: String) {
        self.builder.start_node(ERROR.into());
        self.errors.push(msg);
        self.builder.finish_node();
    }

    fn nth(&self, n: usize) -> SyntaxKind {
        let mut n_real: usize = 0;
        let mut m: usize = n;
        loop {
            if n_real + m >= self.tokens.len() {
                return EOF;
            }

            let token = self.tokens[self.tokens.len() - n_real - 1].0;
            if token == WHITESPACE {
                n_real += 1
            } else if m == 0 {
                return token;
            } else {
                m -= 1;
                n_real += 1;
            }
        }
    }

    /// Peek at the first unprocessed token
    fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    fn nth_at(&self, n: usize, kind: SyntaxKind) -> bool {
        self.nth(n) == kind
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.current() == kind
    }

    fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.current() != kind {
            return false;
        }
        self.skip_ws();
        self.bump_any();
        true
    }

    /// Advance one token, adding it to the current branch of the tree builder.
    fn bump_any(&mut self) {
        if self.current() == EOF {
            return;
        }
        let (kind, text) = self.tokens.pop().unwrap();
        self.builder.token(kind.into(), text);
    }

    fn bump(&mut self, kind: SyntaxKind) {
        assert!(self.eat(kind))
    }

    fn skip_ws(&mut self) {
        while self.tokens.last().map(|t| t.0) == Some(WHITESPACE) {
            self.bump_any()
        }
    }

    // fn expect(&mut self, kind: SyntaxKind) -> bool {
    //     if self.eat(kind) {
    //         return true;
    //     }
    //     self.error(format!("expected {:?}", kind));
    //     false
    // }
}

pub fn parse(text: &str) -> Parse {
    let mut tokens = lex(text);
    tokens.reverse();
    Parser {
        tokens,
        builder: GreenNodeBuilder::new(),
        errors: Vec::new(),
    }
    .parse()
}

pub type SyntaxNode = rowan::SyntaxNode<Lang>;
#[allow(unused)]
type SyntaxToken = rowan::SyntaxToken<Lang>;
#[allow(unused)]
type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

impl Parse {
    fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    pub fn root(&self) -> Root {
        Root::cast(self.syntax()).unwrap()
    }
}

/// Split the input string into a flat list of tokens
/// (such as L_PAREN, WORD, and WHITESPACE)
pub fn lex(text: &str) -> Vec<(SyntaxKind, SmolStr)> {
    fn tok(t: SyntaxKind) -> m_lexer::TokenKind {
        m_lexer::TokenKind(rowan::SyntaxKind::from(t).0)
    }
    fn kind(t: m_lexer::TokenKind) -> SyntaxKind {
        match t.0 {
            0 => L_PAREN,
            1 => R_PAREN,
            2 => LAM,
            3 => ARROW,
            4 => WORD,
            5 => WHITESPACE,
            6 => ERROR,
            _ => unreachable!(),
        }
    }

    let lexer = m_lexer::LexerBuilder::new()
        .error_token(tok(ERROR))
        .tokens(&[
            (tok(L_PAREN), r"\("),
            (tok(R_PAREN), r"\)"),
            (tok(LAM), r"\\"),
            (tok(ARROW), r"->"),
            (tok(WORD), r"[\w]+"),
            (tok(WHITESPACE), r"\s+"),
        ])
        .build();

    lexer
        .tokenize(text)
        .into_iter()
        .map(|t| (t.len, kind(t.kind)))
        .scan(0usize, |start_offset, (len, kind)| {
            let s: SmolStr = text[*start_offset..*start_offset + len].into();
            *start_offset += len;
            Some((kind, s))
        })
        .collect()
}

macro_rules! ast_node {
    ($ast:ident, $kind:ident) => {
        #[derive(PartialEq, Eq, Hash)]
        #[repr(transparent)]
        pub struct $ast(pub SyntaxNode);
        impl $ast {
            #[allow(unused)]
            fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == $kind {
                    Some(Self(node))
                } else {
                    None
                }
            }
        }
    };
}

ast_node!(Root, ROOT);
ast_node!(Var, VAR);
ast_node!(Lambda, LAMBDA);
ast_node!(Application, APPLICATION);

#[derive(PartialEq, Eq, Hash, Debug)]
#[repr(transparent)]
pub struct Expr(SyntaxNode);

pub enum ExprKind {
    Var(Var),
    Lambda(Lambda),
    Application(Application),
}

impl Expr {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if Var::cast(node.clone()).is_some()
            || Lambda::cast(node.clone()).is_some()
            || Application::cast(node.clone()).is_some()
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
            .unwrap()
    }
}

impl Root {
    pub fn expr(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

impl Lambda {
    pub fn body(&self) -> Option<Expr> {
        self.0.children().find_map(Expr::cast)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::{assert_debug_snapshot, glob};

    #[test]
    fn test() {
        use std::fs;

        glob!("examples/*.l3", |path| {
            let input = fs::read_to_string(path).unwrap();
            let parse = parse(&input);
            let node = parse.syntax();

            assert_debug_snapshot!((input, parse.errors, node));
        });
    }
}
