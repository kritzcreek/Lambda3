use super::*;
use crate::lexer::SyntaxKind::*;

/// The outcome of parsing a single S-expression
pub enum ExprRes {
    /// An expression was successfully parsed
    Ok,
    /// An unexpected token was found
    Lul(String),
}

pub fn expr(p: &mut Parser) -> ExprRes {
    let mut is_application = false;
    let checkpoint = p.checkpoint();
    match atom(p) {
        None => return ExprRes::Lul("Expected expression".to_string()),
        Some(ExprRes::Ok) => (),
        Some(ExprRes::Lul(err)) => {
            p.errors.push(err.clone());
            return ExprRes::Lul(err);
        }
    }
    loop {
        match atom(p) {
            None => break,
            Some(ExprRes::Lul(s)) => {
                p.report_error(s);
            }
            Some(ExprRes::Ok) => {
                p.start_node_at(checkpoint, ApplicationE);
                is_application = true;
            }
        }
    }
    if is_application {
        p.finish_node();
    }
    ExprRes::Ok
}

// Option<ExprRes>
// Some(OK) => atom, continue
// Some(Err) => handle recovery
// None => no atom, no tokens

fn atom(p: &mut Parser) -> Option<ExprRes> {
    match p.current() {
        LParen => parse_parenthesized(p),
        Ident => parse_var(p),
        NumberLit | TrueKw | FalseKw => parse_literal(p),
        Backslash => return parse_lambda(p),
        LexerError => p.bump_any(),
        _ => return None,
    }

    Some(ExprRes::Ok)
}

fn parse_parenthesized(p: &mut Parser) {
    p.start_node(ParenE);
    p.bump(LParen);
    expr(p);

    if !p.eat(RParen) {
        p.start_node(Error);
        p.errors.push(format!(
            "unexpected token {:?}, expected ')'",
            p.current()
        ));

        while p.current() != RParen && p.current() != Eof {
            p.bump_any()
        }

        p.finish_node();
        p.eat(RParen);
    }

    p.finish_node();
}

fn parse_var(p: &mut Parser) {
    p.start_node(VarE);
    p.bump(Ident);
    p.finish_node();
}

fn parse_literal(p: &mut Parser) {
    p.start_node(LiteralE);
    p.bump_any();
    p.finish_node();
}

fn parse_lambda(p: &mut Parser) -> Option<ExprRes> {
    p.start_node(LambdaE);
    p.bump(Backslash);

    let checkpoint = p.checkpoint();
    if p.eat(Ident) {
        p.start_node_at(checkpoint, VarP);
        p.finish_node();
    } else {
        p.report_error(format!("expected binder, got {:?}", p.current()))
    }

    if p.eat(Colon) {
        types::typ(p)
    }

    if !p.eat(Arrow) {
        p.finish_node();
        return Some(ExprRes::Lul(format!(
            "expected '->', got {:?}",
            p.current()
        )));
    }

    match expr(p) {
        ExprRes::Ok => p.finish_node(),
        ExprRes::Lul(err) => {
            p.report_error(err);
            p.finish_node();
            return None;
        }
    }

    Some(ExprRes::Ok)
}

