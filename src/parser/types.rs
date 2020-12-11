use super::*;
use crate::lexer::SyntaxKind::*;

pub fn typ(p: &mut Parser) {
    let cp = p.checkpoint();
    if !atom(p) {
        p.report_error("expected type".to_string())
    }

    if p.eat(Arrow) {
        typ(p);
        p.finish_at(cp, FuncTy)
    }
}

fn atom(p: &mut Parser) -> bool {
    match p.current() {
        Int => {
            p.start_node(IntTy);
            p.bump(Int);
            p.finish_node();
        }
        Bool => {
            p.start_node(BoolTy);
            p.bump(Bool);
            p.finish_node()
        }
        LParen => {
            p.start_node(ParenTy);
            p.bump(LParen);
            typ(p);
            if !p.eat(RParen) {
                p.report_error("missing closing paren".to_string())
            }
            p.finish_node()
        }
        _ => return false
    }
    true
}
