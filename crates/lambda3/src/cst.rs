use crate::parser::{self, Parse};

pub fn parse(text: &str) -> Parse {
    parser::parse(text)
}

pub fn parse_type(text: &str) -> Parse {
    parser::parse_type(text)
}

pub fn parse_pattern(text: &str) -> Parse {
    parser::parse_pattern(text)
}
