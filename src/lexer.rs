use logos::Logos;
use num_derive::{FromPrimitive, ToPrimitive};
use rowan::SyntaxNode;

pub(crate) fn lex_str<'a>(input: &'a str) -> Vec<Item<'a>> {
    let mut tokens = vec![];
    let mut lexer = Lexer::new(input);

    while let Some(item) = lexer.next() {
        if item.1.0 == SyntaxKind::Eof {
            tokens.push(item);
            return tokens
        } else {
            tokens.push(item);
        }
    }
    unreachable!()
}

pub(crate) struct Lexer<'a> {
    inner: logos::Lexer<'a, SyntaxKind>,
    lookahead: Option<(SyntaxKind, &'a str)>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            inner: SyntaxKind::lexer(input),
            lookahead: None,
        }
    }
}

fn is_whitespace(kind: SyntaxKind) -> bool {
    kind == SyntaxKind::Whitespace
}

pub(crate) type Item<'a> = (
    Vec<(SyntaxKind, &'a str)>,
    (SyntaxKind, &'a str),
);

impl<'a> Iterator for Lexer<'a> {
    type Item = (
        Vec<(SyntaxKind, &'a str)>,
        (SyntaxKind, &'a str),
    );

    fn next(&mut self) -> Option<Self::Item> {
        let mut leading = vec![];

        loop {
            let kind = self.inner.next();
            let text = self.inner.slice();

            match kind {
                None => return Some((leading, (SyntaxKind::Eof, ""))),
                Some(kind) => {
                    if !is_whitespace(kind) {
                        return Some((leading, (kind, text)))
                    }
                    else {
                        leading.push((kind, text));
                    }
                }
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Logos, FromPrimitive, ToPrimitive)]
#[repr(u16)]
pub(crate) enum SyntaxKind {
    #[regex(r"\s")]
    Whitespace,

    #[token("let")]
    LetKw,

    #[token("true")]
    TrueKw,

    #[token("false")]
    FalseKw,

    #[regex("[A-Za-z][A-Za-z0-9]*")]
    Ident,

    #[regex("[0-9]+")]
    NumberLit,

    #[token(".")]
    Dot,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("\\")]
    Backslash,

    #[token("=")]
    Equals,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("<")]
    Lt,

    #[token(">")]
    Gt,

    #[token("?")]
    Question,

    #[token(":")]
    Colon,

    #[token(";")]
    Semicolon,

    #[token(",")]
    Comma,

    #[token("->")]
    Arrow,

    #[error]
    LexerError,

    Eof,


    // Expressions
    ParenE, // a parenthesized expression
    LiteralE,     // a literal
    VarE,         // wraps a WORD token
    LambdaE,        // a func expression
    ApplicationE, // a function application

    Root,

    // Patterns
    VarP,

}
