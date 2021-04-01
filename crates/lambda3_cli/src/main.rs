use lambda3::cst;
use lambda3::types::Typechecker;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::fs;

fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let input = match line.strip_prefix(":file ") {
                    None => line,
                    Some(filename) => fs::read_to_string(filename).unwrap(),
                };

                let expr = cst::parse(input.as_str()).expr();
                println!("{:#?}", expr);
                let tc = Typechecker {
                    top_level: expr.clone(),
                };
                match tc.infer_expr() {
                    Ok(expr_ast) => {
                        println!("{}", expr_ast);
                        println!("{}", expr_ast.ty());
                    }
                    Err(dl) => {
                        println!("{}", dl);
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
