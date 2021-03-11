mod ast_src;
pub mod codegen;

pub use anyhow::{bail, Context as _, Result};

use std::{
    env,
    path::{Path, PathBuf},
};

pub fn project_root() -> PathBuf {
    Path::new(
        &env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned()),
    )
    .ancestors()
    .nth(1)
    .unwrap()
    .to_path_buf()
}
