use rustyline::error::ReadlineError;
use rustyline::Editor;

use lambda3::cst;
use lambda3::types::infer_expr;

fn main() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let expr = cst::parse(line.as_str()).root().expr().unwrap();
                println!("{:#?}", expr);
                match infer_expr(expr) {
                    Ok(expr_ast) => {
                        println!("{}", expr_ast);
                        println!("{}", expr_ast.ty());
                    }
                    Err(e) => {
                        println!("TypeErr: {}", e);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}
