use rowan::{Checkpoint, GreenNode, GreenNodeBuilder, Language};

use crate::cst;
use crate::lexer::SyntaxKind::{EOF, ERROR, ROOT};
use crate::lexer::{lex_str, SyntaxKind, Token};
use crate::syntax::ast::AstNode;
use crate::syntax::nodes;
use crate::syntax::{Lang, SyntaxNode};

mod expressions;
mod patterns;
mod types;

/// The parse results are stored as a "green tree".
/// We'll discuss working with the results later
#[derive(Debug)]
pub struct Parse {
    pub green_node: GreenNode,
    #[allow(unused)]
    pub errors: Vec<String>,
}

pub struct Parser {
    /// input tokens, including whitespace,
    /// in *reverse* order.
    tokens: Vec<(Vec<Token>, Token)>,
    /// the in-progress tree.
    builder: GreenNodeBuilder<'static>,
    /// the list of syntax errors we've accumulated
    /// so far.
    errors: Vec<String>,
}

impl Parse {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    pub fn expr(&self) -> nodes::Expr {
        self.syntax()
            .children()
            .find_map(|n| nodes::Expr::cast(n))
            .unwrap()
    }

    pub fn debug_tree(&self) -> String {
        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let formatted = format!("{:#?}", syntax_node);

        // We cut off the last byte because formatting the SyntaxNode adds on a newline at the end.
        formatted[0..formatted.len() - 1].to_string()
    }
}

impl Parser {
    fn new(tokens: Vec<(Vec<Token>, Token)>) -> Self {
        Parser {
            tokens,
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),
        }
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(Lang::kind_to_raw(kind));
    }

    fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        self.builder
            .start_node_at(checkpoint, Lang::kind_to_raw(kind));
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    fn finish_at(&mut self, c: Checkpoint, kind: SyntaxKind) {
        self.start_node_at(c, kind);
        self.finish_node();
    }

    fn checkpoint(&self) -> Checkpoint {
        self.builder.checkpoint()
    }

    fn report_error(&mut self, msg: String) {
        self.start_node(ERROR);
        self.errors.push(msg);
        self.finish_node();
    }

    fn nth(&self, n: usize) -> SyntaxKind {
        let len = self.tokens.len();
        if n >= len {
            SyntaxKind::EOF
        } else {
            self.tokens[len - n - 1].1 .0
        }
    }

    /// Peek at the first unprocessed token
    fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.current() == kind
    }

    fn eat(&mut self, kind: SyntaxKind) -> bool {
        if !self.at(kind) {
            return false;
        }
        self.bump_any();
        true
    }

    /// Advance one token, adding it to the current branch of the tree builder.
    fn bump_any(&mut self) {
        if self.at(EOF) {
            return;
        }
        let (leading, (kind, text)) = self.tokens.pop().unwrap();

        for (kind, text) in leading {
            self.builder.token(Lang::kind_to_raw(kind), text.as_str());
        }

        self.builder.token(Lang::kind_to_raw(kind), text.as_str());
    }

    fn bump(&mut self, kind: SyntaxKind) {
        assert!(self.eat(kind))
    }
}

pub fn parse(text: &str) -> Parse {
    let mut tokens = lex_str(text);
    tokens.reverse();
    let mut p = Parser::new(tokens);

    p.start_node(ROOT);
    let _ = expressions::expr(&mut p);
    if !p.at(SyntaxKind::EOF) {
        p.start_node(ERROR);
        while !p.at(EOF) {
            p.bump_any()
        }
        p.finish_node();
        p.errors.push(format!("Unexpected token {:?}", p.current()))
    }
    p.finish_node();

    Parse {
        green_node: p.builder.finish(),
        errors: p.errors,
    }
}

pub fn parse_type(text: &str) -> Parse {
    let mut tokens = lex_str(text);
    tokens.reverse();
    let mut p = Parser::new(tokens);

    p.start_node(ROOT);
    let _ = types::typ(&mut p);
    if !p.at(SyntaxKind::EOF) {
        p.start_node(ERROR);
        while !p.at(EOF) {
            p.bump_any()
        }
        p.finish_node();
        p.errors.push(format!("Unexpected token {:?}", p.current()))
    }
    p.finish_node();

    Parse {
        green_node: p.builder.finish(),
        errors: p.errors,
    }
}

pub fn parse_pattern(text: &str) -> Parse {
    let mut tokens = lex_str(text);
    tokens.reverse();
    let mut p = Parser::new(tokens);

    p.start_node(ROOT);
    let _ = patterns::parse_pattern(&mut p);
    if !p.at(SyntaxKind::EOF) {
        p.start_node(ERROR);
        while !p.at(EOF) {
            p.bump_any()
        }
        p.finish_node();
        p.errors.push(format!("Unexpected token {:?}", p.current()))
    }
    p.finish_node();

    Parse {
        green_node: p.builder.finish(),
        errors: p.errors,
    }
}
