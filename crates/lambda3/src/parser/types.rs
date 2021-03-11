use super::*;
use crate::lexer::SyntaxKind::*;

pub fn typ(p: &mut Parser) {
    let cp = p.checkpoint();
    if !atom(p) {
        p.report_error("expected type".to_string())
    }
    if p.at(ARROW) {
        p.finish_at(cp, TY_ARG);
        p.bump(ARROW);
        let cp2 = p.checkpoint();
        typ(p);
        p.finish_at(cp2, TY_RES);
        p.finish_at(cp, FUNC_TY)
    }
}

fn atom(p: &mut Parser) -> bool {
    match p.current() {
        INT => {
            p.start_node(INT_TY);
            p.bump(INT);
            p.finish_node();
        }
        BOOL => {
            p.start_node(BOOL_TY);
            p.bump(BOOL);
            p.finish_node()
        }
        L_PAREN => {
            p.start_node(PAREN_TY);
            p.bump(L_PAREN);
            typ(p);
            if !p.eat(R_PAREN) {
                p.report_error("missing closing paren".to_string())
            }
            p.finish_node()
        }
        _ => return false,
    }
    true
}
