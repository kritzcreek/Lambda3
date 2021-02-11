use lambda3::ast;
use lambda3::cst::{self, Expr, ExprKind};

fn main() {
    println!("Ok cool");
    let expr = cst::parse("x").root().expr().unwrap();

    let expr = cst::parse("\\x -> x").root().expr().unwrap();
    let expr = cst::parse("\\x -> x x").root().expr().unwrap();
    let expr = cst::parse("\\x -> x 42").root().expr().unwrap();
    let expr = cst::parse("\\x -> false true").root().expr().unwrap();
    //let _ = dbg!(ast::Expr::from_cst(dbg!(expr)));

    //     match expr.kind() {
    //         ExprKind::Var(var) => {}
    //         ExprKind::Lambda(lambda) => {
    // /*            println!("{:#?}", lambda.body());
    //             println!("{:#?}", lambda.0.parent());*/
    //
    //             match lambda.body().unwrap().kind() {
    //                 ExprKind::Var(_) => {}
    //                 ExprKind::Lambda(lambda) => {
    //                     println!("{:#?}", lambda.body());
    //                     println!("{:#?}", lambda.0.parent().unwrap());
    //                     println!("{:#?}", lambda.0.text());
    //                 }
    //                 ExprKind::Application(_) => {}
    //             }
    //         }
    //         ExprKind::Application(app) => {}
    //    }
}
