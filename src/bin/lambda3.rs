use lambda3::{Expr, ExprKind};

fn main() {
    println!("Ok cool");
    let expr = lambda3::parse("\\x -> \\ -> x y").root().expr().unwrap();

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
