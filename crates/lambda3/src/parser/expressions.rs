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
        let arg_checkpoint = p.checkpoint();
        match atom(p) {
            None => break,
            Some(ExprRes::Lul(s)) => {
                p.report_error(s);
            }
            Some(ExprRes::Ok) => {
                //TODO: something's broken
                p.finish_at(arg_checkpoint, EXPR_ARG);
                p.start_node_at(checkpoint, APPLICATION_E);
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
        L_PAREN => parse_parenthesized(p),
        IDENT => parse_var(p),
        NUMBER_LIT | TRUE_KW | FALSE_KW => parse_literal(p),
        BACKSLASH => return parse_lambda(p),
        LET_KW => parse_let(p),
        LEXER_ERROR => p.bump_any(),
        _ => return None,
    }

    Some(ExprRes::Ok)
}

fn parse_let(p: &mut Parser) {
    p.start_node(LET_E);
    p.bump(LET_KW);
    patterns::parse_pattern(p);
    if !p.eat(EQUALS) {
        p.report_error(format!(
            "expected '=' in let expression but got {:?}",
            p.current()
        ))
    }
    if let ExprRes::Lul(err) = expr(p) {
        p.start_node(ERROR);
        p.errors.push(err);
        while !p.eat(IN_KW) {}
        p.finish_node();
    }

    if !p.eat(IN_KW) {
        p.report_error(format!(
            "expected 'in' in let expression but got {:?}",
            p.current()
        ))
    }

    p.start_node(EXPR_LET_BODY);
    if let ExprRes::Lul(err) = expr(p) {
        p.report_error(err)
    };
    p.finish_node();

    p.finish_node();
}

fn parse_parenthesized(p: &mut Parser) {
    p.start_node(PAREN_E);
    p.bump(L_PAREN);
    expr(p);

    if !p.eat(R_PAREN) {
        p.start_node(ERROR);
        p.errors
            .push(format!("unexpected token {:?}, expected ')'", p.current()));

        while !(p.at(R_PAREN) || p.at(EOF)) {
            p.bump_any()
        }

        p.finish_node();
        p.eat(R_PAREN);
    }

    p.finish_node();
}

fn parse_var(p: &mut Parser) {
    p.start_node(VAR_E);
    p.bump(IDENT);
    p.finish_node();
}

fn parse_literal(p: &mut Parser) {
    p.start_node(LITERAL_E);
    p.bump_any();
    p.finish_node();
}

fn parse_lambda(p: &mut Parser) -> Option<ExprRes> {
    p.start_node(LAMBDA_E);
    p.bump(BACKSLASH);

    patterns::parse_pattern(p);

    if !p.eat(DOT) {
        p.finish_node();
        return Some(ExprRes::Lul(format!("expected '.', got {:?}", p.current())));
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
