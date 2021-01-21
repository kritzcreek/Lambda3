use super::*;
use crate::lexer::SyntaxKind::*;

pub fn parse_pattern(p: &mut Parser) {
    let checkpoint = p.checkpoint();

    let current = p.current();
    match current {
        Ident => {
            p.bump(Ident);
            p.finish_at(checkpoint, VarP);
        }
        Underscore => {
            p.bump(Underscore);
            p.finish_at(checkpoint, WildcardP);
        }
        LParen => {
            p.bump(LParen);
            parse_pattern(p);
            if !p.eat(RParen) {
                p.report_error(format!("missing closing paren, got {:?}", p.current()));
            }
            p.finish_at(checkpoint, ParenP);
        }
        _ => {
            p.report_error(format!("expected pattern, got {:?}", current));
            return;
        }
    }

    opt_annotation(p, checkpoint);
}

fn opt_annotation(p: &mut Parser, checkpoint: Checkpoint) -> bool {
    if p.eat(Colon) {
        types::typ(p);
        p.finish_at(checkpoint, AnnotationP);
        return true;
    }

    return false;
}
