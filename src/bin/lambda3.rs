use lambda3::cst::{Expr, ExprKind, self};

fn main() {
    println!("Ok cool");
    let expr = cst::parse("\\x -> \\ -> x y").root().expr().unwrap();

    match expr.kind() {
        ExprKind::Var(var) => {}
        ExprKind::Lambda(lambda) => {
/*            println!("{:#?}", lambda.body());
            println!("{:#?}", lambda.0.parent());*/

            match lambda.body().unwrap().kind() {
                ExprKind::Var(_) => {}
                ExprKind::Lambda(lambda) => {
                    println!("{:#?}", lambda.body());
                    println!("{:#?}", lambda.0.parent().unwrap());
                    println!("{:#?}", lambda.0.text());
                }
                ExprKind::Application(_) => {}
            }
        }
        ExprKind::Application(app) => {}
    }
}
