use insta::{assert_debug_snapshot, glob};

use lambda3::cst::*;

#[test]
fn parse_expr_test() {
    use std::fs;

    glob!("examples/expr/*.l3", |path| {
        let input = fs::read_to_string(path).unwrap();
        let parse = parse(&input);
        let node = parse.syntax();

        assert_debug_snapshot!((input, parse.errors, node));
    });
}

#[test]
fn parse_type_test() {
    use std::fs;

    glob!("examples/types/*.l3", |path| {
        let input = fs::read_to_string(path).unwrap();
        let parse = parse_type(&input);
        let node = parse.syntax();

        assert_debug_snapshot!((input, parse.errors, node));
    });
}

#[test]
fn parse_patterns_test() {
    use std::fs;

    glob!("examples/patterns/*.l3", |path| {
        let input = fs::read_to_string(path).unwrap();
        let parse = parse_pattern(&input);
        let node = parse.syntax();

        assert_debug_snapshot!((input, parse.errors, node));
    });
}
