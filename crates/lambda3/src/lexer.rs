use logos::Logos;
use num_derive::{FromPrimitive, ToPrimitive};
use smol_str::SmolStr;

pub fn lex_str(input: &str) -> Vec<Item> {
    let mut tokens = vec![];
    let lexer = Lexer::new(input);

    for item in lexer {
        if (item.1).0 == SyntaxKind::EOF {
            tokens.push(item);
            return tokens;
        } else {
            tokens.push(item);
        }
    }
    unreachable!()
}

pub struct Lexer<'a> {
    inner: logos::Lexer<'a, SyntaxKind>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            inner: SyntaxKind::lexer(input),
        }
    }
}

fn is_whitespace(kind: SyntaxKind) -> bool {
    kind == SyntaxKind::WHITESPACE
}

pub type Token = (SyntaxKind, SmolStr);
pub type Item = (Vec<Token>, Token);

impl<'a> Iterator for Lexer<'a> {
    type Item = (Vec<Token>, Token);

    fn next(&mut self) -> Option<Self::Item> {
        let mut leading = vec![];

        loop {
            let kind = self.inner.next();
            let text = self.inner.slice();

            match kind {
                None => return Some((leading, (SyntaxKind::EOF, "".into()))),
                Some(kind) => {
                    if !is_whitespace(kind) {
                        return Some((leading, (kind, text.into())));
                    } else {
                        leading.push((kind, text.into()));
                    }
                }
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Logos, FromPrimitive, ToPrimitive)]
#[repr(u16)]
#[allow(bad_style, missing_docs, unreachable_pub)]
pub enum SyntaxKind {
    #[regex(r"\s")]
    WHITESPACE,

    #[token("let")]
    LET_KW,

    #[token("true")]
    TRUE_KW,

    #[token("false")]
    FALSE_KW,

    #[token("Bool")]
    BOOL,

    #[token("Int")]
    INT,

    #[regex("[A-Za-z][A-Za-z0-9]*")]
    IDENT,

    #[regex("[0-9]+")]
    NUMBER_LIT,

    #[token(".")]
    DOT,

    #[token("+")]
    PLUS,

    #[token("-")]
    MINUS,

    #[token("*")]
    STAR,

    #[token("/")]
    SLASH,

    #[token("\\")]
    BACKSLASH,

    #[token("=")]
    EQUALS,

    #[token("(")]
    L_PAREN,

    #[token(")")]
    R_PAREN,

    #[token("[")]
    L_BRACKET,

    #[token("]")]
    R_BRACKET,

    #[token("{")]
    L_BRACE,

    #[token("}")]
    R_BRACE,

    #[token("<")]
    LT,

    #[token(">")]
    GT,

    #[token("?")]
    QUESTION,

    #[token(":")]
    COLON,

    #[token(";")]
    SEMICOLON,

    #[token(",")]
    COMMA,

    #[token("->")]
    ARROW,

    #[token("_")]
    UNDERSCORE,

    #[error]
    LEXER_ERROR,
    ERROR,

    EOF,

    // Types
    PAREN_TY,
    INT_TY,
    BOOL_TY,
    FUNC_TY,

    TY_ARG,
    TY_RES,

    // Expressions
    PAREN_E,       // a parenthesized expression
    LITERAL_E,     // a literal
    VAR_E,         // wraps a WORD token
    LAMBDA_E,      // a func expression
    APPLICATION_E, // a function application

    ROOT,

    // Patterns
    PAREN_P,
    VAR_P,
    WILDCARD_P,
    ANNOTATION_P,
}

#[macro_export]
macro_rules ! T {
    [;] => { SyntaxKind::SEMICOLON };
    [,] => { SyntaxKind::COMMA };
    ['('] => { SyntaxKind::L_PAREN };
    [')'] => { SyntaxKind::R_PAREN };
    ['{'] => { SyntaxKind::L_BRACE };
    ['}'] => { SyntaxKind::R_BRACE };
    ['['] => { SyntaxKind::L_BRACKET };
    [']'] => { SyntaxKind::R_BRACKET };
    [<] => { SyntaxKind::L_ANGLE };
    [>] => { SyntaxKind::R_ANGLE };
    [backslash] => { SyntaxKind::BACKSLASH};
    [#] => { SyntaxKind::HASH };
    [?] => { SyntaxKind::QUESTION };
    [+] => { SyntaxKind::PLUS };
    [*] => { SyntaxKind::STAR };
    [/] => { SyntaxKind::SLASH };
    [^] => { SyntaxKind::CARET };
    [%] => { SyntaxKind::PERCENT };
    [_] => { SyntaxKind::UNDERSCORE };
    [.] => { SyntaxKind::DOT };
    [:] => { SyntaxKind::COLON };
    [=] => { SyntaxKind::EQUALS };
    [==] => { SyntaxKind::DOUBLE_EQUALS };
    [!] => { SyntaxKind::BANG };
    [-] => { SyntaxKind::MINUS };
    [->] => { SyntaxKind::ARROW };
    [<:] => { SyntaxKind::SUB };
    [actor] => { SyntaxKind::ACTOR_KW };
    [class] => { SyntaxKind::CLASS_KW };
    [object] => { SyntaxKind::OBJECT_KW };
    [async] => { SyntaxKind::ASYNC_KW };
    [true] => { SyntaxKind::TRUE_KW };
    [false] => { SyntaxKind::FALSE_KW };
    [null] => { SyntaxKind::NULL_KW };
    [func] => { SyntaxKind::FUNC_KW };
    [for] => { SyntaxKind::FOR_KW };
    [if] => { SyntaxKind::IF_KW };
    [else] => { SyntaxKind::ELSE_KW };
    [let] => { SyntaxKind::LET_KW };
    [switch] => { SyntaxKind::SWITCH_KW };
    [module] => { SyntaxKind::MODULE_KW };
    [var] => { SyntaxKind::VAR_KW };
    [prim] => { SyntaxKind::PRIM_KW };
    [query] => { SyntaxKind::QUERY_KW };
    [shared] => { SyntaxKind::SHARED_KW };
    [return] => { SyntaxKind::RETURN_KW };
    [true] => { SyntaxKind::TRUE_KW };
    [try] => { SyntaxKind::TRY_KW };
    [type] => { SyntaxKind::TYPE_KW };
    [while] => { SyntaxKind::WHILE_KW };
    [number_lit] => { SyntaxKind::NUMBER_LIT };
    [ident] => { SyntaxKind::IDENT };
    [Int] => { SyntaxKind::INT };
    [Bool] => { SyntaxKind::BOOL };
}
