use super::*;
use crate::lexer::SyntaxKind::*;

pub fn parse_pattern(p: &mut Parser) {
    let checkpoint = p.checkpoint();

    let current = p.current();
    match current {
        IDENT => {
            p.bump(IDENT);
            p.finish_at(checkpoint, VAR_P);
        }
        UNDERSCORE => {
            p.bump(UNDERSCORE);
            p.finish_at(checkpoint, WILDCARD_P);
        }
        L_PAREN => {
            p.bump(L_PAREN);
            parse_pattern(p);
            if !p.eat(R_PAREN) {
                p.report_error(format!("missing closing paren, got {:?}", p.current()));
            }
            p.finish_at(checkpoint, PAREN_P);
        }
        _ => {
            p.report_error(format!("expected pattern, got {:?}", current));
            return;
        }
    }

    opt_annotation(p, checkpoint);
}

fn opt_annotation(p: &mut Parser, checkpoint: Checkpoint) -> bool {
    if p.eat(COLON) {
        types::typ(p);
        p.finish_at(checkpoint, ANNOTATION_P);
        return true;
    }

    false
}
